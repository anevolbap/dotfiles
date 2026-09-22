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
;; file prompt, when the selection has not been moved, take the input
;; literally. This lets you create a file whose typed name is a prefix
;; of an existing match (e.g. type "report" when "report-final.org"
;; exists) instead of opening that match. To open an existing file,
;; navigate to it first (C-n) or complete with TAB, then RET. Mirrors
;; vertico-preselect 'prompt.
(defun ao/icomplete--literal-input-p ()
  "Non-nil when RET should take the typed input over the selected candidate.
Moving the selection (C-n, C-p) settles the question: the pick is deliberate,
so RET takes the candidate and the highlight stays on. Until then, empty input
means empty, since `icomplete-force-complete-and-exit' inserts the top
candidate whatever the field holds, so with no text there is no way to clear a
field, e.g. removing every tag at an org `C-c C-c' prompt puts the first tag
back."
  (and (not icomplete--scrolled-completions)
       (or (string-empty-p (minibuffer-contents))
           minibuffer-completing-file-name)))

(defun ao/icomplete-ret ()
  "Exit with the selected candidate, or with the literal input."
  (interactive)
  (if (ao/icomplete--literal-input-p)
      (exit-minibuffer)
    (icomplete-force-complete-and-exit)))
(define-key icomplete-minibuffer-map (kbd "RET") #'ao/icomplete-ret)
;; M-RET exits with the literal input (vertico's M-RET).
(define-key icomplete-minibuffer-map (kbd "M-RET") #'icomplete-fido-exit)
;; TAB inserts the selected candidate without exiting (vertico's TAB).
(define-key icomplete-minibuffer-map (kbd "TAB") #'icomplete-force-complete)

;; From the prompt nothing is selected yet: the highlight is hidden and RET
;; takes the input. `icomplete-forward-completions' pops the head of the list,
;; so the first C-n there would step over the first candidate and land on the
;; second. Mark the list as scrolled instead, leaving its head in place, which
;; selects the first candidate. Vertico's `prompt' preselect moves this way.
(defun ao/icomplete-forward-completions ()
  "Step forward one candidate, or select the first one from the prompt."
  (interactive)
  (if (ao/icomplete--literal-input-p)
      (setq icomplete--scrolled-completions
            (completion-all-sorted-completions (icomplete--field-beg)
                                               (icomplete--field-end)))
    (icomplete-forward-completions)))
(define-key icomplete-vertical-mode-minibuffer-map (kbd "C-n")
            #'ao/icomplete-forward-completions)
(define-key icomplete-vertical-mode-minibuffer-map (kbd "<down>")
            #'ao/icomplete-forward-completions)

;; icomplete always highlights its top candidate. Where RET takes the
;; literal input instead (see ao/icomplete--literal-input-p) that highlight
;; is misleading: it shows e.g. ".profile" as selected while RET would
;; create "file", or a tag as selected while RET would clear the field.
;; Hide it in those cases. Elsewhere the highlight stays, since there RET
;; does take the candidate.
(defvar-local ao/icomplete--noselect-cookie nil)
(defun ao/icomplete--sync-selection-highlight ()
  "Show the top-candidate highlight only when it reflects what RET picks."
  (if (ao/icomplete--literal-input-p)
      (unless ao/icomplete--noselect-cookie
        (setq ao/icomplete--noselect-cookie
              (face-remap-add-relative 'icomplete-selected-match 'default)))
    (when ao/icomplete--noselect-cookie
      (face-remap-remove-relative ao/icomplete--noselect-cookie)
      (setq ao/icomplete--noselect-cookie nil))))
(add-hook 'icomplete-minibuffer-setup-hook
          (lambda ()
            ;; Run late (depth 90) so icomplete-exhibit has refreshed
            ;; icomplete--scrolled-completions before we read it.
            (add-hook 'post-command-hook
                      #'ao/icomplete--sync-selection-highlight 90 t)
            (ao/icomplete--sync-selection-highlight)))

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

;; ------------------------------------------------------------
;; 7. In-buffer completion preview (built-in, Emacs 30+)
;; ------------------------------------------------------------
;; Shows the most likely completion as inline grey text after point.
;; No popup, so it does not hit the GTK cast assertion that made corfu
;; unusable on this build (see the DEPRECATED block in eglot-config.el).
;; Candidates come from `completion-at-point-functions', so eglot and
;; cape feed it in the buffers where they are active.
(use-package completion-preview
  :ensure nil
  :demand t
  :custom
  ;; Preview after 2 characters instead of 3.
  (completion-preview-minimum-symbol-length 2)
  ;; The preview matches on prefix only. `completion-styles' stays as
  ;; configured above for TAB and the minibuffer.
  (completion-preview-completion-styles '(basic))
  :hook ((prog-mode org-mode) . completion-preview-mode)
  :config
  ;; The preview only updates after a command in `completion-preview-commands',
  ;; which lists `self-insert-command'. Org remaps typing and deletion to its
  ;; own commands, so without these the preview never appears in org buffers.
  (dolist (cmd '(org-self-insert-command
                 org-delete-backward-char
                 org-delete-char))
    (add-to-list 'completion-preview-commands cmd))
  :bind
  (:map completion-preview-active-mode-map
        ;; TAB keeps its usual meaning (indent, then show *Completions*)
        ;; rather than accepting the preview, which is on M-RET.
        ("TAB" . completion-preview-complete)
        ("M-RET" . completion-preview-insert)
        ("M-i" . completion-preview-insert-word)
        ("M-n" . completion-preview-next-candidate)
        ("M-p" . completion-preview-prev-candidate)))

;; Richer annotations in the *Completions* buffer
(setq completions-detailed t)

(provide 'completion-config)
;;; completion-config.el ends here
