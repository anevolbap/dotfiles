;;; claude-sessions.el --- Cross-project Claude Code session list -*- lexical-binding: t -*-

;;; Commentary:

;; Lists Claude Code sessions from every project in one collapsible buffer.
;; agent-shell's own picker only offers sessions whose cwd matches the current
;; project, so there is no way to see what is in flight across repos.
;;
;; Sessions come from the ACP adapter's `session/list', which returns all of
;; them when the request carries no cwd filter.  The one-line summary is the
;; last prompt recorded in the session transcript.
;;
;; Nothing in a transcript marks a session as finished, so status cannot be
;; derived in general.  It is assigned by hand and stored in
;; `claude-sessions-status-file'.  The exception is a session that opened a
;; pull request: `claude-sessions-refresh-prs' asks GitHub for its state and
;; maps it through `claude-sessions-pr-status-map'.  A status set by hand
;; always wins over a derived one.
;;
;; M-x claude-sessions
;;   TAB  fold or unfold the project or session at point
;;   RET  resume the session at point in an `agent-shell'
;;   t    set the status of the session at point
;;   x    show or hide finished sessions
;;   g    refresh
;;   P    look up pull request states on GitHub

;;; Code:

(require 'acp)
(require 'cl-lib)
(require 'iso8601)
(require 'magit-section)
(require 'map)

(declare-function agent-shell-resume-session "agent-shell" (session-id))

(defgroup claude-sessions nil
  "Cross-project Claude Code session list."
  :group 'tools)

(defcustom claude-sessions-command "claude-agent-acp"
  "ACP adapter used to query sessions."
  :type 'string
  :group 'claude-sessions)

(defcustom claude-sessions-statuses
  '(("WORKING"   . font-lock-keyword-face)
    ("WAITING"   . warning)
    ("REVIEW"    . font-lock-function-name-face)
    ("DONE"      . success)
    ("CANCELLED" . shadow))
  "Status keywords and the face each is displayed with.
Faces inherit from the theme rather than hardcoding colors, so the
list follows light and dark themes."
  :type '(alist :key-type string :value-type face)
  :group 'claude-sessions)

(defcustom claude-sessions-finished-statuses '("DONE" "CANCELLED")
  "Statuses treated as finished, hidden unless `x' says otherwise."
  :type '(repeat string)
  :group 'claude-sessions)

(defcustom claude-sessions-status-file
  (locate-user-emacs-file "claude-sessions-status.eld")
  "File storing the status assigned to each session id."
  :type 'file
  :group 'claude-sessions)

(defcustom claude-sessions-pr-cache-file
  (locate-user-emacs-file "claude-sessions-pr.eld")
  "File caching the GitHub state of pull requests referenced by sessions."
  :type 'file
  :group 'claude-sessions)

(defcustom claude-sessions-pr-status-map
  '(("OPEN"   . "REVIEW")
    ("DRAFT"  . "WORKING")
    ("MERGED" . "DONE")
    ("CLOSED" . "CANCELLED"))
  "Status keyword implied by each pull request state.
DRAFT stands for an open pull request still marked as a draft.  A status
set by hand always wins over one derived this way."
  :type '(alist :key-type string :value-type string)
  :group 'claude-sessions)

(defcustom claude-sessions-summary-width 90
  "Maximum width of the one-line summary under each session."
  :type 'integer
  :group 'claude-sessions)

(defcustom claude-sessions-tail-bytes 65536
  "How many bytes to read from the end of a transcript.
Transcripts reach tens of megabytes, so only the tail is scanned for
the last recorded prompt."
  :type 'integer
  :group 'claude-sessions)

(defvar-local claude-sessions--index nil
  "Hash table mapping session id to its session alist.")

(defvar-local claude-sessions--show-finished nil
  "When non-nil, show sessions whose status is finished.")

;;;; Data

(defun claude-sessions--fetch ()
  "Return every Claude Code session as a vector of alists."
  (unless (executable-find claude-sessions-command)
    (user-error "%s not found: npm install -g @agentclientprotocol/claude-agent-acp"
                claude-sessions-command))
  (let ((client (acp-make-client :command claude-sessions-command)))
    (unwind-protect
        (progn
          (acp-send-request
           :client client
           :request (acp-make-initialize-request
                     :protocol-version 1
                     :read-text-file-capability t
                     :write-text-file-capability t)
           :sync t)
          ;; `acp-make-session-list-request' insists on a :cwd, which would
          ;; scope the result to a single project.  Empty params ask for every
          ;; session instead, and must be a hash table so `json-serialize'
          ;; emits {} rather than dropping the key (the adapter rejects a
          ;; missing params object).
          (map-elt (acp-send-request
                    :client client
                    :request `((:method . "session/list")
                               (:params . ,(make-hash-table)))
                    :sync t)
                   'sessions))
      (acp-shutdown :client client))))

(defun claude-sessions--transcript (session-id)
  "Return the transcript file for SESSION-ID, or nil.
Transcripts live under a per-project directory whose name encodes the
cwd, so the file is located by name rather than by rebuilding that
encoding."
  (car (file-expand-wildcards
        (expand-file-name (format "~/.claude/projects/*/%s.jsonl" session-id)))))

(defun claude-sessions--scan-back (marker fn)
  "Search backward for MARKER, returning the first non-nil FN of a parsed line.
FN receives the line parsed as an alist.  The scanned tail usually begins
mid-line, and that fragment simply fails to parse, as does any line
merely mentioning MARKER in prose."
  (save-excursion
    (goto-char (point-max))
    (let (value)
      (while (and (not value) (search-backward marker nil t))
        (setq value
              (ignore-errors
                (funcall fn (json-parse-string
                             (buffer-substring-no-properties
                              (line-beginning-position) (line-end-position))
                             :object-type 'alist
                             :null-object nil
                             :false-object nil)))))
      value)))

(defun claude-sessions--transcript-facts (session-id)
  "Return (PROMPT . PR) read from the tail of SESSION-ID's transcript.
PROMPT is the last prompt recorded, which makes a better summary than the
generated title.  PR is (REPOSITORY . NUMBER) taken from the most recent
`pr-link' entry, or nil.  Both are read from a single pass.

Transcripts reach tens of megabytes, so only the last
`claude-sessions-tail-bytes' are scanned.  A pull request linked earlier
than that is not seen; on the transcripts this was written against, the
last `pr-link' sits within 33KB of the end in 18 of 19 cases.

Note this reads an undocumented on-disk format, unlike the rest of this
file which goes through ACP.  A Claude Code update may change it, hence
the fallbacks."
  (when-let* ((file (claude-sessions--transcript session-id))
              (size (file-attribute-size (file-attributes file))))
    (with-temp-buffer
      (let ((coding-system-for-read 'utf-8))
        (insert-file-contents file nil (max 0 (- size claude-sessions-tail-bytes)) size))
      (cons (claude-sessions--scan-back
             "\"last-prompt\"" (lambda (entry) (map-elt entry 'lastPrompt)))
            (claude-sessions--scan-back
             "\"pr-link\""
             (lambda (entry)
               (when-let* ((repo (map-elt entry 'prRepository))
                           (number (map-elt entry 'prNumber)))
                 (cons repo number))))))))

(defun claude-sessions--summarize (text)
  "Collapse TEXT to a single truncated line."
  (when (and text (not (string-empty-p (string-trim text))))
    (truncate-string-to-width
     (string-trim (replace-regexp-in-string "[ \t\n\r]+" " " text))
     claude-sessions-summary-width 0 nil t)))

;;;; Status

(defun claude-sessions--read-eld (file)
  "Return the Lisp datum stored in FILE, or nil."
  (when (file-readable-p file)
    (ignore-errors
      (with-temp-buffer
        (insert-file-contents file)
        (read (current-buffer))))))

(defun claude-sessions--write-eld (file data)
  "Write DATA to FILE."
  (with-temp-file file
    (let ((print-length nil)
          (print-level nil))
      (prin1 data (current-buffer))
      (insert "\n"))))

(defun claude-sessions--load-statuses ()
  "Return the saved session id to status alist."
  (claude-sessions--read-eld claude-sessions-status-file))

(defun claude-sessions--save-statuses (statuses)
  "Write STATUSES to `claude-sessions-status-file'."
  (claude-sessions--write-eld claude-sessions-status-file statuses))

(defun claude-sessions--status (session-id)
  "Return the status assigned to SESSION-ID, or nil."
  (alist-get session-id (claude-sessions--load-statuses) nil nil #'equal))

(defun claude-sessions--pr-key (pr)
  "Return the cache key for PR, a (REPOSITORY . NUMBER) cons."
  (format "%s#%s" (car pr) (cdr pr)))

(defun claude-sessions--derived-status (pr cache)
  "Return the status implied by PR according to CACHE, or nil."
  (when-let* ((pr)
              (entry (alist-get (claude-sessions--pr-key pr) cache nil nil #'equal))
              (state (plist-get entry :state)))
    (alist-get (if (and (equal state "OPEN") (plist-get entry :draft))
                   "DRAFT"
                 state)
               claude-sessions-pr-status-map nil nil #'equal)))

(defun claude-sessions--finished-p (status)
  "Return non-nil when STATUS counts as finished."
  (and status (member status claude-sessions-finished-statuses) t))

;;;; Grouping

(defun claude-sessions--project-parts (cwd)
  "Return (PROJECT . WORKTREE) for CWD.
WORKTREE is nil unless CWD sits under a project's .claude/worktrees
directory, in which case the sessions belong to PROJECT rather than to a
project of their own."
  (let ((path (directory-file-name (abbreviate-file-name cwd))))
    (if (string-match "/\\([^/]+\\)/\\.claude/worktrees/\\([^/]+\\)\\'" path)
        (cons (match-string 1 path) (match-string 2 path))
      (cons (if (equal path "~") "~" (file-name-nondirectory path))
            nil))))

(defun claude-sessions--format-time (timestamp)
  "Format ISO 8601 TIMESTAMP as a short local time string."
  (condition-case nil
      (let ((time (encode-time (iso8601-parse timestamp))))
        (format-time-string
         (if (equal (format-time-string "%F" time) (format-time-string "%F"))
             "Today %H:%M"
           "%b %d %H:%M")
         time))
    (error timestamp)))

(defun claude-sessions--name< (a b)
  "Compare names A and B alphabetically, ignoring case.
Plain `string<' is ASCII order, which would file every capitalized
project before every lowercase one."
  (string-lessp (downcase a) (downcase b)))

(defun claude-sessions--sort-sessions (sessions)
  "Return SESSIONS newest first."
  (sort (copy-sequence sessions)
        (lambda (a b)
          (string> (or (map-elt a 'updatedAt) "")
                   (or (map-elt b 'updatedAt) "")))))

(defun claude-sessions--group (sessions)
  "Group SESSIONS by project, nesting worktrees under their parent.
Returns a list of (PROJECT OWN-SESSIONS ((WORKTREE . SESSIONS) ...)).
Projects and worktrees are ordered alphabetically, sessions newest
first."
  (let ((table (make-hash-table :test #'equal))
        (result nil))
    (dolist (session (append sessions nil))
      (pcase-let* ((`(,project . ,worktree)
                    (claude-sessions--project-parts (or (map-elt session 'cwd) "")))
                   (entry (or (gethash project table)
                              (puthash project (list nil nil) table))))
        (if worktree
            (push session (alist-get worktree (nth 1 entry) nil nil #'equal))
          (push session (nth 0 entry)))))
    (maphash
     (lambda (project entry)
       (push (list project
                   (claude-sessions--sort-sessions (nth 0 entry))
                   (sort (mapcar (lambda (cell)
                                   (cons (car cell)
                                         (claude-sessions--sort-sessions (cdr cell))))
                                 (nth 1 entry))
                         (lambda (a b) (claude-sessions--name< (car a) (car b)))))
             result))
     table)
    (sort result (lambda (a b) (claude-sessions--name< (car a) (car b))))))

;;;; Rendering

(defun claude-sessions--status-string (status)
  "Return STATUS padded and propertized for display."
  (let ((width (apply #'max 9 (mapcar (lambda (s) (length (car s)))
                                      claude-sessions-statuses))))
    (if status
        (propertize (string-pad status width) 'face
                    (alist-get status claude-sessions-statuses 'default nil #'equal))
      (propertize (string-pad "-" width) 'face 'shadow))))

(defun claude-sessions--insert-session (session)
  "Insert a section for SESSION."
  (let* ((id (map-elt session 'sessionId))
         (status (map-elt session 'status)))
    (magit-insert-section (claude-session id)
      (magit-insert-heading
        (concat "  "
                (claude-sessions--status-string status)
                "  "
                (propertize (claude-sessions--format-time
                             (or (map-elt session 'updatedAt) ""))
                            'face 'shadow)
                "  "
                (if-let* ((pr (map-elt session 'pr)))
                    (propertize (format "#%s " (cdr pr)) 'face 'link)
                  "")
                (or (map-elt session 'title) "(untitled)")))
      (when-let* ((summary (map-elt session 'summary)))
        (insert "                " (propertize (concat "last: " summary) 'face 'shadow) "\n")))))

(defun claude-sessions--insert-worktree (project name sessions)
  "Insert a section for worktree NAME of PROJECT holding SESSIONS.
Collapsed by default, since worktrees are usually side work; the
visibility cache remembers it once unfolded."
  (magit-insert-section (claude-worktree (concat project "/" name) t)
    (magit-insert-heading
      (concat "  "
              (propertize name 'face 'magit-section-secondary-heading)
              " "
              (propertize (format "(%d)" (length sessions)) 'face 'shadow)))
    (dolist (session sessions)
      (claude-sessions--insert-session session))))

(defun claude-sessions--insert-project (project own worktrees)
  "Insert a section for PROJECT holding OWN sessions and WORKTREES."
  (let ((total (+ (length own)
                  (apply #'+ (mapcar (lambda (cell) (length (cdr cell))) worktrees)))))
    (magit-insert-section (claude-project project)
      (magit-insert-heading
        (concat (propertize project 'face 'magit-section-heading)
                " "
                (propertize (format "(%d)" total) 'face 'shadow)))
      (dolist (session own)
        (claude-sessions--insert-session session))
      (pcase-dolist (`(,name . ,sessions) worktrees)
        (claude-sessions--insert-worktree project name sessions)))))

(defun claude-sessions--render ()
  "Redraw the buffer from the ACP adapter."
  (let* ((inhibit-read-only t)
         (statuses (claude-sessions--load-statuses))
         (pr-cache (claude-sessions--read-eld claude-sessions-pr-cache-file))
         (index (make-hash-table :test #'equal))
         (hidden 0)
         (sessions nil))
    ;; Annotate first, so filtering and rendering read the same data.
    (dolist (session (append (claude-sessions--fetch) nil))
      (pcase-let* ((id (map-elt session 'sessionId))
                   (`(,prompt . ,pr) (claude-sessions--transcript-facts id))
                   ;; A status set by hand wins; GitHub only fills the gap.
                   (status (or (alist-get id statuses nil nil #'equal)
                               (claude-sessions--derived-status pr pr-cache))))
        (setf (alist-get 'status session) status)
        (setf (alist-get 'pr session) pr)
        (setf (alist-get 'summary session) (claude-sessions--summarize prompt))
        (puthash id session index)
        (if (and (claude-sessions--finished-p status)
                 (not claude-sessions--show-finished))
            (cl-incf hidden)
          (push session sessions))))
    (setq claude-sessions--index index)
    (erase-buffer)
    (magit-insert-section (claude-sessions-root)
      (magit-insert-heading
        (concat (propertize "Claude sessions" 'face 'magit-section-heading)
                (propertize (format "  %d shown" (length sessions)) 'face 'shadow)
                (if (> hidden 0)
                    (propertize (format ", %d finished hidden (x to show)" hidden)
                                'face 'shadow)
                  "")))
      (pcase-dolist (`(,project ,own ,worktrees)
                     (claude-sessions--group (nreverse sessions)))
        (claude-sessions--insert-project project own worktrees)))
    ;; Inserting a section only records whether it should be hidden; the
    ;; overlay that actually hides it is applied by `magit-section-show'
    ;; walking the tree.  Without this, folds restored from the visibility
    ;; cache are silently ignored on refresh.
    (magit-section-show magit-root-section)
    (goto-char (point-min))))

;;;; Commands

(defun claude-sessions--session-at-point ()
  "Return the session alist at point, or signal an error."
  (let ((id (magit-section-value-if 'claude-session)))
    (unless id
      (user-error "No session at point"))
    (or (gethash id claude-sessions--index)
        (user-error "Session %s is no longer listed" id))))

(defun claude-sessions-resume ()
  "Resume the session at point in an `agent-shell'."
  (interactive)
  (require 'agent-shell)
  (let* ((session (claude-sessions--session-at-point))
         (cwd (map-elt session 'cwd)))
    (unless (and cwd (file-directory-p cwd))
      (user-error "Session directory is gone: %s" cwd))
    ;; `agent-shell' derives its cwd from the current buffer, which here is
    ;; this list.  Point it at the session's own directory instead.
    (let ((default-directory (file-name-as-directory cwd)))
      (agent-shell-resume-session (map-elt session 'sessionId)))))

(defun claude-sessions-set-status (status)
  "Set STATUS on the session at point.
An empty STATUS clears it."
  (interactive
   (list (completing-read "Status (empty to clear): "
                          (mapcar #'car claude-sessions-statuses)
                          nil t)))
  (let* ((session (claude-sessions--session-at-point))
         (id (map-elt session 'sessionId))
         (statuses (claude-sessions--load-statuses)))
    (if (string-empty-p status)
        (setq statuses (assoc-delete-all id statuses))
      (setf (alist-get id statuses nil nil #'equal) status))
    (claude-sessions--save-statuses statuses)
    (let ((pos (point)))
      (claude-sessions--render)
      (goto-char (min pos (point-max))))))

(defun claude-sessions-toggle-finished ()
  "Show or hide sessions whose status is finished."
  (interactive)
  (setq claude-sessions--show-finished (not claude-sessions--show-finished))
  (claude-sessions--render))

(defun claude-sessions--referenced-prs ()
  "Return the distinct pull requests referenced by listed sessions."
  (let ((seen (make-hash-table :test #'equal)))
    (when claude-sessions--index
      (maphash (lambda (_id session)
                 (when-let* ((pr (map-elt session 'pr)))
                   (puthash (claude-sessions--pr-key pr) pr seen)))
               claude-sessions--index))
    (hash-table-values seen)))

(defun claude-sessions-refresh-prs ()
  "Look up the GitHub state of every pull request a session references.
Queries run in parallel and the buffer is redrawn once the last one
returns.  Results are cached, so an ordinary refresh does not hit the
network."
  (interactive)
  (unless (executable-find "gh")
    (user-error "gh not found; cannot look up pull request state"))
  (let* ((buffer (current-buffer))
         (prs (claude-sessions--referenced-prs))
         (cache (claude-sessions--read-eld claude-sessions-pr-cache-file))
         (pending (length prs)))
    (when (zerop pending)
      (user-error "No listed session references a pull request"))
    (message "Looking up %d pull request%s..." pending (if (= pending 1) "" "s"))
    (dolist (pr prs)
      (let ((key (claude-sessions--pr-key pr)))
        (make-process
         :name "claude-sessions-gh"
         :buffer (generate-new-buffer " *claude-sessions-gh*")
         :noquery t
         ;; Over the default pty, gh detects a terminal and wraps its JSON in
         ;; a spinner and ANSI color, which will not parse.
         :connection-type 'pipe
         :command (list "gh" "pr" "view" (number-to-string (cdr pr))
                        "--repo" (car pr) "--json" "state,isDraft")
         :sentinel
         (lambda (process _event)
           (when (memq (process-status process) '(exit signal))
             (with-current-buffer (process-buffer process)
               ;; A failed lookup (deleted repo, no network) leaves the
               ;; previous cache entry alone rather than clearing it.
               (when-let* ((json (ignore-errors
                                   (json-parse-string (buffer-string)
                                                      :object-type 'alist
                                                      :null-object nil
                                                      :false-object nil))))
                 (setf (alist-get key cache nil nil #'equal)
                       (list :state (map-elt json 'state)
                             :draft (eq (map-elt json 'isDraft) t)))))
             (kill-buffer (process-buffer process))
             (cl-decf pending)
             (when (zerop pending)
               (claude-sessions--write-eld claude-sessions-pr-cache-file cache)
               (if (buffer-live-p buffer)
                   (with-current-buffer buffer
                     (claude-sessions--render)
                     (message "Pull request states updated"))
                 (message "Pull request states updated"))))))))))

(defun claude-sessions-refresh ()
  "Reload sessions from the ACP adapter.
Pull request states come from the cache; press \\[claude-sessions-refresh-prs]
to look them up again."
  (interactive)
  (claude-sessions--render))

(defvar-keymap claude-sessions-mode-map
  :doc "Keymap for `claude-sessions-mode'."
  "RET" #'claude-sessions-resume
  "t"   #'claude-sessions-set-status
  "x"   #'claude-sessions-toggle-finished
  "g"   #'claude-sessions-refresh
  "P"   #'claude-sessions-refresh-prs)

(define-derived-mode claude-sessions-mode magit-section-mode "Claude Sessions"
  "Major mode for browsing Claude Code sessions across projects."
  :interactive nil
  (setq-local revert-buffer-function
              (lambda (&rest _) (claude-sessions--render))))

;;;###autoload
(defun claude-sessions ()
  "Show Claude Code sessions across all projects."
  (interactive)
  (let ((buffer (get-buffer-create "*Claude Sessions*")))
    (with-current-buffer buffer
      (claude-sessions-mode)
      (claude-sessions--render))
    (pop-to-buffer buffer)))

(provide 'claude-sessions)

;;; claude-sessions.el ends here
