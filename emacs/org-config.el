;;; org-config.el --- Org mode configuration  -*- lexical-binding: t -*-
;;;
;;; Commentary:
;;; Org agenda, capture, org-roam, org-download.
;;;
;;; Code:

(defconst my-org-directory (expand-file-name "~/org")
  "Main directory for org files.")

(defconst my-org-notes-file (expand-file-name "notes.org" my-org-directory)
  "Default file for org notes.")

(defconst my-org-capture-templates-file
  (expand-file-name "capture-templates" user-emacs-directory)
  "File containing org capture templates.")

(use-package org
  :ensure nil
  :mode ("\\.org\\'" . org-mode)
  :hook ((org-mode . org-fold-hide-block-all)
         (org-mode . visual-line-mode)
         (org-mode . abbrev-mode)
         (org-mode . (lambda () (electric-indent-local-mode -1))))
  :bind (("C-c l"       . org-store-link)
         ("C-c c"       . org-capture)
         ("C-c a"       . org-agenda)
         ("C-c b"       . org-switchb)
         ("C-c C-w"     . org-refile)
         ("C-c j"       . org-clock-goto)
         ("C-c C-x C-o" . org-clock-out))
  :custom
  ;; Directories and files
  (org-directory my-org-directory)
  (org-default-notes-file my-org-notes-file)

  ;; Scan the org directory at agenda-build time, not at load time.
  ;; This picks up new .org files without requiring a restart.
  (org-agenda-files (list my-org-directory))

  ;; Capture templates
  (org-capture-templates
   (when (file-exists-p my-org-capture-templates-file)
     (ao/read-from-file my-org-capture-templates-file)))

  ;; TODO keywords
  (org-todo-keywords
   '((sequence "TODO(t)" "IN-PROGRESS(i)" "WAITING(w)" "|" "DONE(d)" "CANCELLED(c)")))

  ;; Per-keyword colors. Inherit theme faces instead of hardcoding hex, so the
  ;; palette follows the theme (light/dark).
  (org-todo-keyword-faces
   '(("EXPIRED"   . (:inherit shadow))))                ; dead/neutral

  ;; Refile across all agenda files, up to 3 levels deep.
  ;; Not `org-agenda-files' directly: claude-sessions.org is generated and lives
  ;; in the org directory, so it is an agenda file, and nothing should ever be
  ;; refiled into a file that gets rewritten wholesale.
  (org-refile-targets '((ao/refile-files :maxlevel . 3)))
  (org-refile-use-outline-path 'file)
  (org-outline-path-complete-in-steps nil)

  ;; Behavior
  (org-reverse-note-order t)
  (org-hide-emphasis-markers t)
  (org-support-shift-select t)
  (org-pretty-entities t)
  (org-return-follows-link t)
  (org-catch-invisible-edits 'show-and-error)

  ;; Startup
  (org-startup-folded 'content)
  (org-startup-indented t)

  ;; Appearance — org-modern-star replaces heading stars with Unicode glyphs,
  ;; so org-hide-leading-stars is not needed (org-modern handles that itself).
  (org-cycle-separator-lines 0)

  ;; Logging
  (org-log-done 'time)
  (org-log-into-drawer "HISTORY")

  ;; Export
  (org-export-coding-system 'utf-8)

  ;; Agenda
  (org-agenda-start-with-log-mode nil)
  (org-agenda-window-setup 'current-window)
  ;; Tighter agenda layout: no blank lines between blocks, Unicode separator line
  (org-agenda-compact-blocks t)
  (org-agenda-block-separator ?─)
  ;; Right-align tags at column 80
  (org-agenda-tags-column -80)

  ;; Time grid: show hours 8-20 with Unicode separators
  (org-agenda-time-grid
   '((daily today require-timed)
     (800 1000 1200 1400 1600 1800 2000)
     " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄"))
  (org-agenda-current-time-string "◀── now")

  ;; Custom agenda commands
  (org-agenda-custom-commands
   '(("d" "Dashboard"
      ((ao/dashboard-upcoming-block)
       (tags-todo "+read"
                  ((org-agenda-overriding-header "Reading list (top 5)")
                   (org-agenda-sorting-strategy '(todo-state-up priority-down))
                   (org-agenda-max-entries 5)))
       (tags-todo "-read"
                  ((org-agenda-overriding-header "Active TODOs (top 5 by latest update)")
                   (org-agenda-sorting-strategy '(tsia-down))
                   (org-agenda-skip-function
                    '(org-agenda-skip-entry-if 'regexp "^[ \t]*:STYLE:[ \t]+habit"))
                   (org-agenda-max-entries 5)))))
     ("l" "Reading list" tags-todo "read")
     ;; Habits (STYLE: habit in habits.org). Only today's line is shown, with
     ;; the consistency graph; `org-habit-graph-column' sets where it starts.
     ("h" "Habits"
      ((agenda "" ((org-agenda-span 1)
                   (org-agenda-entry-types '(:scheduled))
                   (org-agenda-time-grid nil)
                   (org-agenda-format-date "%A %-e %B %Y")
                   (org-agenda-skip-function
                    '(org-agenda-skip-entry-if 'notregexp "^[ \t]*:STYLE:[ \t]+habit"))
                   (org-agenda-overriding-header "\nHabits\n")))))
     ("u" "Upcoming (next 14 days)"
      ((agenda "" ((org-agenda-span 14)
                   (org-agenda-start-on-weekday nil)
                   (org-agenda-show-all-dates nil)
                   (org-deadline-warning-days 0)
                   (org-agenda-entry-types '(:scheduled :deadline :timestamp))
                   (org-agenda-time-grid nil)
                   (org-agenda-overriding-header "\nUpcoming (+14d)\n")))))
     ("t" "All active TODOs"
      ((todo "TODO|IN-PROGRESS|WAITING"
             ((org-agenda-overriding-header "\nActive TODOs\n")
              (org-agenda-skip-function
               '(org-agenda-skip-entry-if 'regexp "^[ \t]*:STYLE:[ \t]+habit"))))))
     ;; Claude Code sessions. These entries carry no TODO keyword, so they show
     ;; up in no other command here. Grouped by the PROJECT property rather than
     ;; by tags, since org tags cannot contain the hyphen in e.g. my-project.
     ("S" "Claude sessions by project"
      tags "SESSION<>\"\""
      ((org-super-agenda-groups '((:auto-property "PROJECT")))
       (org-agenda-sorting-strategy '(user-defined-down))
       (org-agenda-cmp-user-defined #'ao/agenda-cmp-updated)
       (org-agenda-prefix-format '((tags . "  ")))
       (org-agenda-overriding-header "Claude sessions")))
     ("A" "Daily agenda and top priority tasks"
      ((agenda "" ((org-agenda-span 1)
                   (org-deadline-warning-days 0)
                   (org-scheduled-past-days 0)
                   (org-agenda-day-face-function (lambda (date) 'org-agenda-date))
                   (org-agenda-format-date "%A %-e %B %Y")
                   (org-agenda-skip-function
                    '(org-agenda-skip-entry-if 'todo 'done))
                   (org-agenda-overriding-header "\nToday's agenda\n")))
       (agenda "" ((org-agenda-start-on-weekday 1)
                   (org-agenda-span 14)
                   (org-agenda-show-all-dates nil)
                   (org-deadline-warning-days 365)
                   (org-agenda-entry-types '(:scheduled :deadline))
                   (org-agenda-time-grid nil)
                   (org-agenda-overriding-header "\nUpcoming (+14d)\n")))))))

  :config
  (add-to-list 'org-modules 'org-habit t)
  (add-to-list 'org-modules 'org-protocol t) ; browser capture via org-protocol://

  ;; org-habit: show habits only on today's agenda line; keep graph narrow.
  ;; Undone habits show with the `!' glyph and leave the agenda once marked DONE,
  ;; so an empty day agenda means done for the day. In the agenda, `K' toggles
  ;; habits off/on and `C-u K' shows the graphs of habits already done today.
  ;; A missed habit keeps its old SCHEDULED date, so it would be dropped by the
  ;; `org-scheduled-past-days' 0 in the "A" day block. Habits use this value
  ;; instead, so an overdue habit stays on today's agenda until it is done.
  (setq org-habit-show-habits-only-for-today t
        org-habit-scheduled-past-days 10000
        org-habit-graph-column 50
        org-habit-preceding-days 14
        org-habit-following-days 7)

  ;; Source block abbrev: type "bgs" + SPC to insert a src block
  (define-abbrev org-mode-abbrev-table "bgs" ""
    (lambda ()
      (let ((lang (read-string "Language: " "emacs-lisp")))
        (insert (format "#+begin_src %s\n\n#+end_src" lang))
        (forward-line -1)
        (end-of-line)))))

;; ============================================================================
;; Claude Code sessions in the agenda
;;
;; Sessions live in claude-sessions.org, written by M-x claude-sessions-org-sync.
;; They are plain entries whose only timestamp sits in the :UPDATED: property,
;; which keeps them out of every todo and date view but also defeats the stock
;; recency sorts: `tsia-down' reads TIMESTAMP_IA, which is nil for a timestamp
;; inside a property drawer, and is day-granular anyway. Hence a comparator.
;; ============================================================================

(defvar claude-sessions-org-file)
(declare-function claude-sessions-org-sync "claude-sessions-org")

;; org-agenda is deferred, so its options are not special variables when this
;; file is compiled, and `let' binds them lexically instead of dynamically.
;; The agenda functions then never see the bindings and silently use the global
;; values. Declaring them special here fixes every `let' below, including the
;; one in `ao/dashboard-upcoming-block'.
(defvar org-agenda-cmp-user-defined)
(defvar org-agenda-entry-types)
(defvar org-agenda-overriding-header)
(defvar org-agenda-prefix-format)
(defvar org-agenda-show-all-dates)
(defvar org-agenda-sorting-strategy)
(defvar org-agenda-span)
(defvar org-agenda-start-on-weekday)
(defvar org-agenda-time-grid)
(defvar org-scheduled-past-days)

(defun ao/refile-files ()
  "Agenda files minus the generated Claude sessions file.
Excluding the file here rather than with `org-refile-target-verify-function'
also drops the file-level target, which that hook never sees."
  (seq-remove (lambda (file)
                (and (boundp 'claude-sessions-org-file)
                     claude-sessions-org-file
                     (equal (expand-file-name file)
                            (expand-file-name claude-sessions-org-file))))
              (org-agenda-files)))

(defun ao/agenda-updated-time (entry)
  "Return the :UPDATED: time of agenda line ENTRY in seconds, or nil."
  (when-let* ((marker (or (get-text-property 0 'org-hd-marker entry)
                          (get-text-property 0 'org-marker entry)))
              (value (org-entry-get marker "UPDATED")))
    (ignore-errors (org-time-string-to-seconds value))))

(defun ao/agenda-cmp-updated (a b)
  "Compare agenda entries A and B by :UPDATED:, most recent first.
Returns +1, -1 or nil as `org-agenda-cmp-user-defined' requires."
  (let ((ta (ao/agenda-updated-time a))
        (tb (ao/agenda-updated-time b)))
    (cond ((and ta tb) (cond ((> ta tb) +1)
                             ((< ta tb) -1)))
          (ta +1)
          (tb -1))))

(defun ao/session-projects ()
  "Return the distinct :PROJECT: values in `claude-sessions-org-file'."
  (require 'claude-sessions-org)
  (unless (file-readable-p claude-sessions-org-file)
    (user-error "No session file yet; run M-x claude-sessions-org-sync"))
  (let (projects)
    (with-temp-buffer
      (insert-file-contents claude-sessions-org-file)
      (let ((org-inhibit-startup t)
            (org-mode-hook nil))
        (org-mode))
      (org-map-entries
       (lambda ()
         (when-let* ((project (org-entry-get nil "PROJECT")))
           (cl-pushnew project projects :test #'equal)))))
    (sort projects (lambda (a b) (string-lessp (downcase a) (downcase b))))))

(defun ao/sessions-for-project (project)
  "Show Claude Code sessions for PROJECT, most recent first."
  (interactive (list (completing-read "Project: " (ao/session-projects) nil t)))
  (let ((org-agenda-sorting-strategy '(user-defined-down))
        (org-agenda-cmp-user-defined #'ao/agenda-cmp-updated)
        (org-agenda-prefix-format '((tags . "  ")))
        (org-agenda-overriding-header (format "Sessions: %s" project)))
    (org-tags-view nil (format "PROJECT=\"%s\"" project))))

;; Dashboard: upcoming agenda block that always inserts its section header.
;; An (agenda "") block silently omits its overriding-header when it produces
;; no entries; this wrapper inserts the header unconditionally.
(defun ao/dashboard-upcoming-block (&optional _match)
  "14-day deadline/scheduled block: always shows its header even when empty."
  (let ((org-agenda-span 14)
        (org-agenda-start-on-weekday nil)
        (org-agenda-show-all-dates nil)
        (org-deadline-warning-days 0)
        (org-scheduled-past-days 0)
        (org-agenda-entry-types '(:deadline :scheduled))
        (org-agenda-time-grid nil)
        (org-agenda-overriding-header nil))
    (let ((inhibit-read-only t))
      (goto-char (point-max))
      (insert "Upcoming (14 days)\n"))
    (let ((before (point-max)))
      (org-agenda-list)
      (when (= (point-max) before)
        (let ((inhibit-read-only t))
          (goto-char (point-max))
          (insert "  (no items)\n"))))))

;; ============================================================================
;; Link triage — the "t" capture template fills :URL: from the clipboard
;; automatically when it looks like a link, untagged. Running
;; `ao/org-triage-links' (by hand, or via the timer below) classifies each
;; untagged link TODO in tasks.org by regex on its URL: arxiv -> paper
;; (title/author/venue/year fetched from the arXiv API, filed into
;; papers.org), youtube/vimeo -> video, anything else -> read. Video and read
;; entries are just tagged in place.
;; ============================================================================

(require 'xml)

(defun ao/clipboard-url-or-nil ()
  "Return the current kill if it looks like a URL, else nil.
Used by the \"t\" capture template so a plain task capture does not pick up
unrelated clipboard text as a bogus :URL: property."
  (let ((s (ignore-errors (current-kill 0 t))))
    (and s (string-match-p "\\`https?://" s) s)))

(defun ao/arxiv-id-from-url (url)
  "Return the arxiv id in URL, or nil if URL is not an arxiv link."
  (when (and url (string-match "arxiv\\.org/\\(?:abs\\|pdf\\)/\\([0-9]+\\.[0-9]+\\)" url))
    (match-string 1 url)))

(defun ao/arxiv-fetch-metadata (id)
  "Fetch title, authors and year for arxiv ID from the arXiv API."
  (let ((buf (url-retrieve-synchronously
              (format "https://export.arxiv.org/api/query?id_list=%s" id)
              t t 15)))
    (unless buf
      (error "No response fetching arxiv metadata for %s" id))
    (unwind-protect
        (with-current-buffer buf
          (goto-char (point-min))
          (re-search-forward "\n\n")
          (let* ((feed (car (xml-parse-region (point) (point-max))))
                 (entry (car (xml-get-children feed 'entry))))
            (unless entry
              (error "No entry in arxiv response for %s" id))
            (list
             :title (xml-substitute-special
                     (string-trim (car (xml-node-children (car (xml-get-children entry 'title))))))
             :authors (mapconcat
                       (lambda (author)
                         (car (xml-node-children (car (xml-get-children author 'name)))))
                       (xml-get-children entry 'author)
                       ", ")
             :year (let ((published (car (xml-node-children (car (xml-get-children entry 'published))))))
                     (and published (substring published 0 4))))))
      (kill-buffer buf))))

(defun ao/org-file-under (file heading)
  "Return a marker at HEADING (top level) in FILE, creating both if needed."
  (with-current-buffer (find-file-noselect file)
    (goto-char (point-min))
    (unless (re-search-forward (format "^\\* %s$" (regexp-quote heading)) nil t)
      (goto-char (point-max))
      (unless (bolp) (insert "\n"))
      (insert (format "* %s\n" heading)))
    (point-marker)))

(defun ao/org-take-subtree ()
  "Delete the subtree at point and return its text.
Deliberately avoids `org-cut-subtree': that cuts with `kill-region', which
appends to the previous kill entry once `last-command' is `kill-region'.
Called in a loop, the kill then accumulates every subtree cut so far and
`org-paste-subtree' re-inserts the whole pile each time."
  (org-back-to-heading t)
  (let* ((beg (point))
         (end (save-excursion (org-end-of-subtree t t) (point)))
         (text (buffer-substring-no-properties beg end)))
    (delete-region beg end)
    text))

(defun ao/org-triage-paper-entry (marker papers-file)
  "Fill arxiv metadata for the paper entry at MARKER, then file it under PAPERS-FILE."
  (let ((subtree
         (with-current-buffer (marker-buffer marker)
           (goto-char marker)
           (let* ((url (org-entry-get nil "URL"))
                  (id (ao/arxiv-id-from-url url)))
             (unless id
               (user-error "No arxiv id in URL: %s" url))
             (let ((meta (ao/arxiv-fetch-metadata id)))
               (org-edit-headline (plist-get meta :title))
               (org-entry-put marker "AUTHOR" (plist-get meta :authors))
               (org-entry-put marker "VENUE" "arXiv")
               (org-entry-put marker "YEAR" (plist-get meta :year))
               (org-entry-put marker "DOI" (format "https://arxiv.org/abs/%s" id))))
           (goto-char marker)
           (ao/org-take-subtree))))
    (let ((target (ao/org-file-under papers-file "Papers")))
      (with-current-buffer (marker-buffer target)
        (goto-char target)
        (end-of-line)
        (newline)
        ;; Pass the text explicitly; never read it back from the kill ring.
        (org-paste-subtree 2 subtree)
        (save-buffer)))))

(defun ao/classify-link-url (url)
  "Classify URL as `paper', `video' or `read' by pattern match."
  (cond
   ((string-match-p "arxiv\\.org/" url) 'paper)
   ((string-match-p "\\(?:youtube\\.com\\|youtu\\.be\\|vimeo\\.com\\)/" url) 'video)
   (t 'read)))

(defun ao/org-triage-links ()
  "Classify untagged link TODOs in tasks.org and tag or file them.
Papers get arxiv metadata filled in and are moved into papers.org; video and
read entries are just tagged in place. Logs rather than stops on an entry
that fails."
  (interactive)
  (let* ((tasks-file (expand-file-name "tasks.org" my-org-directory))
         (papers-file (expand-file-name "papers.org" my-org-directory))
         (buf (find-file-noselect tasks-file))
         (markers (with-current-buffer buf
                    (delq nil
                          (org-map-entries
                           (lambda ()
                             (when (and (equal (org-get-todo-state) "TODO")
                                        (not (string= (or (org-entry-get nil "URL") "") ""))
                                        (not (org-get-tags nil t)))
                               (point-marker)))
                           nil 'file))))
         (done 0) (failed 0))
    (dolist (marker markers)
      (condition-case err
          (progn
            (with-current-buffer (marker-buffer marker)
              (goto-char marker)
              (let* ((url (org-entry-get nil "URL"))
                     (type (ao/classify-link-url url)))
                (org-set-tags (list (symbol-name type)))
                (when (eq type 'paper)
                  (ao/org-triage-paper-entry marker papers-file))))
            (cl-incf done))
        (error (cl-incf failed)
               (message "ao/org-triage-links: failed at %s: %s"
                        marker (error-message-string err)))))
    (with-current-buffer buf (save-buffer))
    (message "ao/org-triage-links: %d tagged, %d failed" done failed)))

;; DEPRECATED: automatic timer, disabled after real-data testing turned up
;; entries messier than the synthetic fixtures covered. Run `M-x
;; ao/org-triage-links' by hand for now.
;; (run-with-timer 60 (* 6 60 60) #'ao/org-triage-links)

;; ============================================================================
;; org-download — drag-and-drop / paste images into org files
;; Images are saved alongside the org file and linked automatically.
;; Usage: org-download-clipboard  (paste from clipboard)
;;        org-download-yank        (yank image URL)
;; ============================================================================

(use-package org-download
  :ensure t
  :after org
  :hook (dired-mode . org-download-enable)
  :custom
  ;; Save images in an ./img/ subdirectory next to the org file
  (org-download-method 'directory)
  (org-download-image-dir "img")
  (org-download-heading-lvl nil)   ; don't nest by heading
  (org-download-timestamp "%Y%m%d_%H%M%S_")
  :bind (:map org-mode-map
              ("C-c i c" . org-download-clipboard)
              ("C-c i y" . org-download-yank)
              ("C-c i s" . org-download-screenshot)))

;; ============================================================================
;; org-roam — personal knowledge base / zettelkasten
;; ============================================================================

(use-package org-roam
  :ensure t
  ;; Defer until first roam command — avoids starting the SQLite DB watcher
  ;; on every startup when no roam files are being visited.
  :commands (org-roam-node-find org-roam-node-insert org-roam-buffer-toggle
             org-roam-dailies-capture-today org-roam-dailies-goto-today)
  :custom
  (org-roam-directory (concat my-org-directory "/roam-notes"))
  (org-roam-dailies-directory "daily/")
  (org-roam-dailies-capture-templates
   '(("d" "default" entry "* %U %?"
      :target (file+head "%<%Y-%m-%d>.org" "#+title: %<%Y-%m-%d>\n"))))
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n j" . org-roam-dailies-capture-today)
         ("C-c n t" . org-roam-dailies-goto-today))
  :config
  (require 'org-roam-dailies)
  (org-roam-db-autosync-mode))

;; ============================================================================
;; ox-reveal — org export to reveal.js presentations
;; ============================================================================

(use-package ox-reveal
  :defer t)

;; ============================================================================
;; org-modern — replace bullets, TODO keywords, tags with Unicode glyphs
;; ============================================================================

(use-package org-modern
  :ensure t
  :after org
  :custom
  ;; Stars: use a slim unicode bullet instead of asterisks
  (org-modern-star '("◉" "○" "◈" "◇" "▷"))
  ;; TODO keywords: inherit face colours, add padding
  (org-modern-todo t)
  (org-modern-tag t)
  (org-modern-timestamp t)
  (org-modern-priority t)
  (org-modern-block-name t)
  (org-modern-keyword t)
  ;; Table styling
  (org-modern-table t)
  :config
  (global-org-modern-mode))

;; ============================================================================
;; org-super-agenda — group agenda items into labelled sections
;; ============================================================================

(use-package org-super-agenda
  :ensure t
  :after org-agenda
  :custom
  ;; No global grouping — each custom command sets its own org-super-agenda-groups.
  (org-super-agenda-groups nil)
  :config
  (org-super-agenda-mode))

;; ============================================================================
;; Agenda face customizations (modus-vivendi compatible)
;; ============================================================================

(with-eval-after-load 'org-agenda
  ;; Date header: stand out but not too loud
  (set-face-attribute 'org-agenda-date nil
                      :weight 'bold :height 1.05)
  (set-face-attribute 'org-agenda-date-today nil
                      :weight 'bold :height 1.1 :underline t)
  ;; Weekend dates: muted blue-grey to distinguish from weekdays without shouting
  (set-face-attribute 'org-agenda-date-weekend nil
                      :weight 'normal :slant 'italic :foreground "steel blue")
  ;; Make done items clearly de-emphasised
  (set-face-attribute 'org-agenda-done nil
                      :strike-through t))

;; Show empty agenda blocks with a placeholder so the section header is
;; still visible (org-agenda hides empty block bodies otherwise).
(defun ao/agenda-mark-empty-blocks ()
  "Insert \"  (no items)\" under any empty block in the current agenda.
A block is considered empty when its header line is immediately followed
by another block-separator line (or end of buffer)."
  (when (and (eq major-mode 'org-agenda-mode)
             (characterp org-agenda-block-separator))
    (let* ((sep (char-to-string org-agenda-block-separator))
           (sep-re (concat "^" (regexp-quote sep) "+$"))
           (inhibit-read-only t)
           (mark-empty
            (lambda ()
              (let ((header-end (line-end-position)))
                (forward-line 1)
                (when (or (eobp) (looking-at-p sep-re))
                  (goto-char header-end)
                  (insert "\n  (no items)"))))))
      (save-excursion
        ;; First block has no separator before it; treat the first non-blank,
        ;; non-separator line at point-min as a header.
        (goto-char (point-min))
        (while (and (not (eobp)) (looking-at-p "^[ \t]*$"))
          (forward-line 1))
        (unless (or (eobp) (looking-at-p sep-re))
          (funcall mark-empty))
        ;; Subsequent blocks: each separator line precedes a header line.
        (goto-char (point-min))
        (while (re-search-forward sep-re nil t)
          (forward-line 1)
          (funcall mark-empty))))))

(add-hook 'org-agenda-finalize-hook #'ao/agenda-mark-empty-blocks)

;; Collapsible blocks in org-agenda via outline-minor-mode.
;; Block headers are flush-left lines ending in "(...)" — e.g.
;; "Upcoming (14 days)", "Reading list (top 5)". Items and date headers
;; don't match, so only the dashboard-style block titles are fold points.
(defun ao/agenda-setup-folding ()
  "Enable outline-minor-mode in org-agenda for collapsible block sections."
  (when (eq major-mode 'org-agenda-mode)
    (setq-local outline-regexp "^[A-Z][^\n]*([^)]*)\\s-*$")
    (setq-local outline-level (lambda () 1))
    (outline-minor-mode 1)))

(add-hook 'org-agenda-mode-hook #'ao/agenda-setup-folding)

(defun ao/agenda-fold-or-goto ()
  "On section headers: fold/unfold.  On items: visit the entry."
  (interactive)
  (if (and (bound-and-true-p outline-minor-mode)
           (save-excursion (beginning-of-line) (looking-at-p outline-regexp)))
      (outline-cycle)
    (call-interactively #'org-agenda-goto)))

(with-eval-after-load 'org-agenda
  ;; TAB: smart — folds headers, visits items.
  ;; S-TAB: cycle all sections at once.
  (define-key org-agenda-mode-map (kbd "<tab>")     #'ao/agenda-fold-or-goto)
  (define-key org-agenda-mode-map (kbd "<backtab>") #'outline-cycle-buffer))

(provide 'org-config)
;;; org-config.el ends here
