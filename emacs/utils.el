;;; utils.el --- Utility functions  -*- lexical-binding: t -*-
;;;
;;; Commentary:
;;; Auto-tangle hook for settings.org, ao/read-from-file helper,
;;; and miscellaneous interactive utilities.
;;;
;;; Code:

;; Automatically tangle settings.org to settings.el on save
(defvar ao/emacs-org-config-file
  (expand-file-name "settings.org" user-emacs-directory)
  "Absolute path to my Emacs configuration Org file.")

(defun ao/org-babel-tangle-config ()
  ;; Use file-equal-p instead of string-equal so the comparison works whether
  ;; the buffer is visiting the dotfiles source path or the stow symlink in
  ;; ~/.emacs.d/ — both resolve to the same inode.
  (when (and (buffer-file-name)
             (file-equal-p (buffer-file-name) ao/emacs-org-config-file))
    (org-babel-tangle)))
(add-hook 'after-save-hook #'ao/org-babel-tangle-config)

(defun ao/read-from-file (file)
  "Read and return the first Lisp expression from FILE.
Used to load external data files (webjump sites, elfeed feeds, etc.)
without requiring them to be Emacs Lisp source files."
  (with-temp-buffer
    (insert-file-contents file)
    (read (current-buffer))))

(defun ao/calculate-reading-time ()
  "Calculate the estimated reading time for the current buffer."
  (interactive)
  (let* ((num-words (count-words-region (point-min) (point-max)))
         (reading-speed-constant 200)
         (reading-time (/ num-words reading-speed-constant)))
    (message "Estimated reading time: %.2f minutes" reading-time)))

;; Desktop notifications through D-Bus, rendered by dunst (see dunst/ in the
;; dotfiles repo). dunst parses the text as Pango markup, so it has to be
;; escaped. The requires are inside the function to keep dbus out of startup.

(defun ao/notify (title body &optional urgency)
  "Show a desktop notification with TITLE and BODY.
URGENCY is `low', `normal' (the default) or `critical'."
  (require 'notifications)
  (require 'xml)
  (notifications-notify :title (xml-escape-string title)
                        :body (xml-escape-string body)
                        :app-name "Emacs"
                        :urgency (or urgency 'normal)))

(defvar ao/notify-compilation-threshold 10
  "Only notify about compilations that ran at least this many seconds.
Keeps quick greps and recompiles from popping a notification.")

(defvar ao/notify-compilation-start nil
  "Start time of the running compilation.")

(defun ao/notify-compilation-started (_proc)
  "Remember when a compilation started."
  (setq ao/notify-compilation-start (current-time)))

(defun ao/notify-compilation-finished (buffer status)
  "Notify that compilation in BUFFER ended with STATUS.
Silent for runs shorter than `ao/notify-compilation-threshold'."
  (let ((elapsed (and ao/notify-compilation-start
                      (float-time (time-since ao/notify-compilation-start)))))
    (setq ao/notify-compilation-start nil)
    (when (and elapsed (>= elapsed ao/notify-compilation-threshold))
      (let ((ok (string-prefix-p "finished" status)))
        (ao/notify (if ok "Compilation finished" "Compilation failed")
                   (format "%s in %ds: %s"
                           (buffer-name buffer)
                           (round elapsed)
                           (string-trim status))
                   (if ok 'normal 'critical))))))

(add-hook 'compilation-start-hook #'ao/notify-compilation-started)
(add-hook 'compilation-finish-functions #'ao/notify-compilation-finished)

(provide 'utils)
;;; utils.el ends here
