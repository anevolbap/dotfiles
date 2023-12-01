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

(provide 'utils)
;;; utils.el ends here
