;;; eglot-config.el --- Python development configuration  -*- lexical-binding: t -*-
;;;
;;; Commentary:
;;; Python LSP via ty (type checking, completions, hover, go-to-def).
;;; Ruff linting is handled by ty natively; no separate ruff LSP needed.
;;; Tree-sitter, pyenv, poetry, corfu completion.
;;;
;;; Code:

;; ============================================================================
;; Tree-sitter — grammar sources, mode remapping, font-lock level
;; Kept here alongside eglot/Python config since tree-sitter is a dev tool.
;; ============================================================================

;; Maximum syntax highlighting detail
(setq treesit-font-lock-level 4)

;; Grammar download sources (used by treesit-install-language-grammar)
(setq treesit-language-source-alist
      '((bash       "https://github.com/tree-sitter/tree-sitter-bash")
        (cmake      "https://github.com/uyha/tree-sitter-cmake")
        (css        "https://github.com/tree-sitter/tree-sitter-css")
        (elisp      "https://github.com/Wilfred/tree-sitter-elisp")
        (go         "https://github.com/tree-sitter/tree-sitter-go")
        (html       "https://github.com/tree-sitter/tree-sitter-html")
        (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
        (json       "https://github.com/tree-sitter/tree-sitter-json")
        (make       "https://github.com/alemuller/tree-sitter-make")
        (markdown   "https://github.com/ikatyang/tree-sitter-markdown")
        (python     "https://github.com/tree-sitter/tree-sitter-python")
        (toml       "https://github.com/tree-sitter/tree-sitter-toml")
        (tsx        "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
        (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
        (yaml       "https://github.com/ikatyang/tree-sitter-yaml")))

;; Redirect legacy major modes to their tree-sitter equivalents
(setq major-mode-remap-alist
      '((yaml-mode       . yaml-ts-mode)
        (bash-mode       . bash-ts-mode)
        (js2-mode        . js-ts-mode)
        (typescript-mode . typescript-ts-mode)
        (json-mode       . json-ts-mode)
        (css-mode        . css-ts-mode)
        (python-mode     . python-ts-mode)))

;; Auto-install Python grammar if missing (values 4 are already the defaults).
(when (treesit-available-p)
  (unless (treesit-language-available-p 'python)
    (treesit-install-language-grammar 'python)))

;; ============================================================================
;; Pyenv
;; ============================================================================

(use-package pyenv-mode
  :ensure t
  :hook (python-ts-mode . pyenv-mode))

;; ============================================================================
;; DEPRECATED: Poetry (replaced by uv). Remove after confirming uv workflow.
;; ============================================================================
;; (use-package poetry
;;   :ensure t
;;   :init
;;   (setq poetry-tracking-strategy 'project)
;;   :hook
;;   (python-ts-mode . poetry-tracking-mode))

;; ============================================================================
;; LSP: ty server
;;
;; ty — https://docs.astral.sh/ty/features/language-server/
;;   Handles: hover docs, go-to-definition, completions, type diagnostics,
;;   and ruff linting natively. No initializationOptions needed.
;;   The only valid initializationOptions are logFile and logLevel.
;; ============================================================================

(use-package eglot
  :ensure nil
  ;; python-mode is remapped to python-ts-mode via major-mode-remap-alist,
  ;; so only the ts-mode hook is needed.
  :hook (python-ts-mode . eglot-ensure)

  :bind (:map eglot-mode-map
              ("C-c r"   . eglot-rename)
              ("C-c a"   . eglot-code-actions)
              ("C-c h"   . eldoc-doc-buffer)
              ("C-c C-f" . eglot-format-buffer))

  :custom
  (eglot-events-buffer-size 0)   ; disable event log for performance
  (eglot-sync-connect 1)
  (eglot-autoshutdown t)
  (eglot-connect-timeout 60)

  :config
  ;; eglot-stay-out-of: exclude yasnippet (not installed).
  ;; Keep eldoc so eglot can populate hover docs.
  (setq eglot-stay-out-of '(yasnippet))

  (add-to-list 'eglot-server-programs
               '((python-mode python-ts-mode)
                 . ("uvx" "ty" "server")))

)

;; ============================================================================
;; Flymake — built-in, used by eglot for diagnostics display
;; ============================================================================

(use-package flymake
  :ensure nil
  ;; Don't hook flymake to python-ts-mode directly; eglot enables it automatically.
  ;; Add hooks here for non-eglot modes that need flymake.
  :bind (:map flymake-mode-map
              ("M-n"     . flymake-goto-next-error)
              ("M-p"     . flymake-goto-prev-error)
              ("C-c ! l" . flymake-show-buffer-diagnostics))
  :custom
  (flymake-mode-line-format
   '("" flymake-mode-line-exception flymake-mode-line-counters))
  (flymake-show-diagnostics-at-end-of-line t)
  (flymake-no-changes-timeout 0.5)
  (flymake-start-on-save-buffer t))

;; ============================================================================
;; Breadcrumb — header line showing current context (file > class > function)
;; ============================================================================

(use-package breadcrumb
  :ensure t
  :demand t
  :config
  (setq breadcrumb-max-length 40
        breadcrumb-max-width 0.4
        breadcrumb-imenu-display-depth 3
        breadcrumb-use-ido nil)
  (breadcrumb-mode 1))

;; ============================================================================
;; DEPRECATED: Corfu + corfu-terminal in-GUI workaround
;;   Disabled while testing whether the built-in *Completions* popup is silent
;;   on this build now that tooltips are routed to the echo area. If TAB
;;   completion in any buffer prints a GTK cast warning, re-enable both blocks.
;; ============================================================================

;; (use-package corfu
;;   :ensure t
;;   :demand t  ; global-corfu-mode must run at startup, not on first trigger
;;   :init
;;   (global-corfu-mode)
;;   :custom
;;   (corfu-auto t)
;;   (corfu-auto-delay 0.2)
;;   (corfu-auto-prefix 2)
;;   (corfu-cycle t)
;;   (corfu-preselect 'prompt)
;;   (corfu-quit-no-match 'separator))

;; ;; Force overlay-based popups in both GUI and terminal. corfu's default
;; ;; child-frame popup hits a GTK3 cast assertion on this build; the popon
;; ;; overlay path used by corfu-terminal sidesteps it entirely.
;; (use-package corfu-terminal
;;   :ensure t
;;   :demand t
;;   :init (add-to-list 'warning-suppress-types '(corfu))
;;   :custom (corfu-terminal-disable-on-gui nil)
;;   :config (corfu-terminal-mode +1))

;; ============================================================================
;; Cape — extra completion-at-point backends (file paths, dabbrev)
;; Works with whatever completion UI is active.
;; ============================================================================

(use-package cape
  :ensure t
  :init
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev))

;; ============================================================================
;; Python built-in settings
;; ============================================================================

(use-package python
  :ensure nil
  :mode ("\\.py\\'" . python-ts-mode)
  :custom
  (python-shell-interpreter "python3")
  (python-indent-guess-indent-offset-verbose nil)
  (python-shell-completion-native-enable nil)
)

;;; eglot-config.el ends here
