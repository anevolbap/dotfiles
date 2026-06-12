;;; completion-config.el --- Completion configuration for Emacs  -*- lexical-binding: t -*-

;;; Commentary:
;; Minimal, efficient completion setup using built-in icomplete-vertical
;; + Orderless + Consult. No frills - just fast, predictable completion.

;;; Code:

;; ------------------------------------------------------------
;; 1. Core Emacs Completion Settings
;; ------------------------------------------------------------
;; Note: abbrev-mode is enabled per-mode (org-mode hook in org-config.el),
;; not globally, to avoid interference with prog-mode buffers.
(use-package emacs
  :ensure nil
  :config
  ;; TAB behavior: first press indents, subsequent presses show completions
  (setq tab-always-indent 'complete)

  ;; TAB cycles through candidates when there are 3 or fewer options
  (setq completion-cycle-threshold 3)

  ;; Bind M-TAB for completion (standard Emacs binding)
  (global-set-key (kbd "M-TAB") 'completion-at-point))

;; ------------------------------------------------------------
;; 2. Minibuffer UI (built-in icomplete-vertical)
;; ------------------------------------------------------------
(icomplete-vertical-mode 1)
(setq icomplete-show-matches-on-no-input t
      icomplete-prospects-height 5)
(setq icomplete-scroll t)
;; Truncate long candidates instead of wrapping
(add-hook 'icomplete-minibuffer-setup-hook
          (lambda () (setq-local truncate-lines t)))
;; RET accepts the selected candidate, like vertico. Exception: in a
;; file prompt, when the input is a directory path and the selection
;; has not been moved, take the input literally so RET in C-x d opens
;; the prompted directory instead of the first match inside it (what
;; vertico-preselect 'directory does).
(defun ao/icomplete-ret ()
  "Exit with the selected candidate, or with a literal directory input."
  (interactive)
  (if (and minibuffer-completing-file-name
           (string-suffix-p "/" (minibuffer-contents))
           (not icomplete--scrolled-completions))
      (exit-minibuffer)
    (icomplete-force-complete-and-exit)))
(define-key icomplete-minibuffer-map (kbd "RET") #'ao/icomplete-ret)
;; M-RET exits with the literal input (vertico's M-RET).
(define-key icomplete-minibuffer-map (kbd "M-RET") #'icomplete-fido-exit)
;; TAB inserts the selected candidate without exiting (vertico's TAB).
(define-key icomplete-minibuffer-map (kbd "TAB") #'icomplete-force-complete)

;; ------------------------------------------------------------
;; 3. Flexible Matching (Orderless)
;; ------------------------------------------------------------
(use-package orderless
  :demand t
  :config
  (setq completion-styles '(orderless flex basic)
        completion-category-overrides
        '((file (styles partial-completion basic)))))

;; ------------------------------------------------------------
;; 4. Action System (Embark)
;; ------------------------------------------------------------
(use-package embark
  :bind
  (("C-." . embark-act)         ;; Act on target at point
   ("C-;" . embark-dwim)        ;; Do what I mean
   ("C-h B" . embark-bindings)) ;; Show keybindings
  :init
  ;; Use Embark for key help
  (setq prefix-help-command #'embark-prefix-help-command))

;; ------------------------------------------------------------
;; 5. Enhanced Search & Navigation (Consult)
;; ------------------------------------------------------------
(use-package consult
  :bind
  ;; Essential bindings - keep it simple
  (("C-x b"   . consult-buffer)
   ("C-x 4 b" . consult-buffer-other-window)
   ("M-g g"   . consult-goto-line)
   ("M-g i"   . consult-imenu)        ; searchable imenu (classes/functions/vars)
   ("M-g I"   . consult-imenu-multi)  ; imenu across all project buffers
   ("M-s l"   . consult-line)
   ("M-s g"   . consult-grep)
   ("M-s f"   . consult-flymake)      ; jump between flymake diagnostics
   ("M-y"     . consult-yank-pop)
   ;; Project buffers
   ("C-x p b" . consult-project-buffer))

  :config
  ;; Better register preview
  (setq register-preview-delay 0.3
        register-preview-function #'consult-register-format)

  ;; Use Consult for xref
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  ;; Preview configuration
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   :preview-key '(:debounce 0.4 any)))

;; ------------------------------------------------------------
;; 6. Integration (Embark + Consult)
;; ------------------------------------------------------------
(use-package embark-consult
  :after (embark consult))

;; Richer annotations in the *Completions* buffer
(setq completions-detailed t)

(provide 'completion-config)
;;; completion-config.el ends here
