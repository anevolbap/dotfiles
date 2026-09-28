;;; dashboard.el --- Quick-view dashboard buffer -*- lexical-binding: t; -*-

;; Author: Pablo Vena
;; Description: World-clock + BTC price + weekly agenda in a closable buffer.
;; Usage: M-x dashboard-show (bound to C-c s).  Press q or ESC to dismiss.

;;; Code:

(require 'time)
(require 'org-agenda)

(declare-function btc-price-refresh "btc-price")
(declare-function btc-price--format "btc-price")
(defvar btc-price--current)

(defgroup dashboard nil
  "Quick-view dashboard buffer."
  :group 'convenience
  :prefix "dashboard-")

(defcustom dashboard-agenda-span 7
  "Number of days the agenda section spans."
  :type 'integer
  :group 'dashboard)

(defconst dashboard--buffer-name "*dashboard*")

(defvar dashboard-mode-map
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "q")        #'dashboard-close)
    (define-key m (kbd "<escape>") #'dashboard-close)
    (define-key m (kbd "g")        #'dashboard-refresh)
    m)
  "Keymap for `dashboard-mode'.")

(define-derived-mode dashboard-mode special-mode "Dashboard"
  "Major mode for the dashboard buffer."
  (setq-local cursor-type nil
              truncate-lines t))

(defun dashboard--world-clock-string ()
  "Return current times for every zone in `world-clock-list'."
  (let* ((now (current-time))
         (label-width
          (apply #'max 0 (mapcar (lambda (z) (length (cadr z)))
                                 world-clock-list)))
         ;; Elisp's `format' has no %-*s — bake the width into the format string.
         (fmt (format "  %%-%ds  %%s" label-width)))
    (mapconcat
     (lambda (zone)
       (format fmt (cadr zone)
               (format-time-string world-clock-time-format now (car zone))))
     world-clock-list "\n")))

(defun dashboard--btc-string ()
  "Return a string with the current BTC price.
Value may be stale on first open — `dashboard-show' kicks off a refresh and
re-renders shortly after."
  (cond
   ((not (featurep 'btc-price)) "(btc-price not loaded)")
   (btc-price--current (string-trim (btc-price--format btc-price--current)))
   (t "BTC fetching…")))

(defun dashboard--agenda-string ()
  "Return the next `dashboard-agenda-span' days of `org-agenda' as a string.
If the agenda has no scheduled entries, return a friendly placeholder.
Any org files opened solely to build the agenda are killed afterwards so
they do not pile up in `buffer-list'."
  (let ((pre-buffers (buffer-list)))
    (unwind-protect
        (condition-case err
            (save-window-excursion
              (let ((org-agenda-window-setup 'current-window)
                    (org-agenda-sticky nil)
                    (org-agenda-inhibit-startup t))
                (org-agenda-list nil nil dashboard-agenda-span))
              (with-current-buffer org-agenda-buffer-name
                (if (text-property-not-all (point-min) (point-max) 'org-marker nil)
                    (let ((s (buffer-string)))
                      ;; Strip agenda's text-property keymaps so our mode-map
                      ;; handles q / ESC instead of local-map taking precedence.
                      (remove-list-of-text-properties
                       0 (length s) '(keymap local-map mouse-face) s)
                      s)
                  "  No commitments soon.")))
          (error (format "  (agenda unavailable: %s)" (error-message-string err))))
      ;; Kill any org file-buffers that were opened just for the agenda scan,
      ;; leaving buffers the user already had open untouched.
      (dolist (buf (buffer-list))
        (when (and (not (memq buf pre-buffers))
                   (buffer-file-name buf)
                   (not (buffer-modified-p buf))
                   (with-current-buffer buf (derived-mode-p 'org-mode)))
          (kill-buffer buf))))))

(defun dashboard--render ()
  "Render dashboard content into `dashboard--buffer-name' and return it."
  (let ((buf (get-buffer-create dashboard--buffer-name)))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (dashboard-mode)
        (insert (propertize "  World Clock\n" 'face 'bold))
        (insert (dashboard--world-clock-string) "\n\n")
        (insert (propertize "  Bitcoin\n" 'face 'bold))
        (insert "  " (dashboard--btc-string) "\n\n")
        (insert (propertize (format "  Agenda (next %d days)\n"
                                    dashboard-agenda-span)
                            'face 'bold))
        (insert (dashboard--agenda-string)))
      (goto-char (point-min)))
    buf))

;;;###autoload
(defun dashboard-show ()
  "Open the dashboard in a full-frame window.
Press \\[dashboard-close] to dismiss (restores the previous window layout),
or \\[dashboard-refresh] to re-fetch and re-render."
  (interactive)
  (when (featurep 'btc-price) (btc-price-refresh))
  (pop-to-buffer (dashboard--render) '((display-buffer-full-frame)))
  ;; BTC fetch is async — re-render shortly to pick up the fresh value.
  (run-with-timer
   1.5 nil
   (lambda ()
     (when (get-buffer-window dashboard--buffer-name 'visible)
       (dashboard--render)))))

(defun dashboard-close ()
  "Dismiss the dashboard and restore the previous window layout."
  (interactive)
  (quit-window))

(defun dashboard-refresh ()
  "Re-render the dashboard contents in place."
  (interactive)
  (when (featurep 'btc-price) (btc-price-refresh))
  (dashboard--render))

(provide 'dashboard)
;;; dashboard.el ends here
