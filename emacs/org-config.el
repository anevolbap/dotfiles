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

  ;; Refile across all agenda files, up to 3 levels deep
  (org-refile-targets '((org-agenda-files :maxlevel . 3)))
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

  ;; org-habit: keep the graph narrow.
  ;; `org-habit-show-habits-only-for-today' nil puts a habit on every agenda day
  ;; it is due, not just today. `org-habit-show-all-today' keeps it on today's
  ;; line even after it is marked DONE, when the `.+1d' repeater has already
  ;; pushed SCHEDULED to tomorrow.
  (setq org-habit-show-habits-only-for-today nil
        org-habit-show-all-today t
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
