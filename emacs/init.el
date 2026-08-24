;; init.el --- Initialization file for Emacs  -*- lexical-binding: t -*-
;;;
;;; Commentary:
;;; Emacs Startup File --- initialization for Emacs
;;;
;;; Code:


(load (concat user-emacs-directory "elpaca-config.el"))

(setq use-package-always-defer t)
;; Enable verbose logging and statistics only when EMACS_DEBUG is set.
;; Usage: EMACS_DEBUG=1 emacs
;; Inspect results with: M-x use-package-report
(let ((debug (getenv "EMACS_DEBUG")))
  (setq use-package-verbose           (when debug t)
        use-package-compute-statistics (when debug t)))

;; Startup profiler — launch with: emacs --eval "(require 'esup) (esup)"
(use-package esup :defer t)

;; Time spent in Emacs startup
(add-hook 'elpaca-after-init-hook
          (lambda ()
            (message "*** Emacs loaded in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)))

;; Additional exec-path entries for tools not on the system PATH.
;; Also keep $PATH in sync so subprocesses (vterm, eshell, sh -c ...) see them.
;; Needed for daemon mode, where $PATH is not inherited from a login shell.
;; nvm is only sourced from .bashrc, so node and its global npm binaries
;; (claude-agent-acp) are invisible to daemon Emacs. Resolve the bin dir of the
;; version named in ~/.nvm/alias/default instead of hardcoding it.
(let* ((alias (expand-file-name "~/.nvm/alias/default"))
       (version (and (file-readable-p alias)
                     (string-trim (with-temp-buffer
                                    (insert-file-contents alias)
                                    (buffer-string)))))
       (nvm-bin (and version
                     (car (last (file-expand-wildcards
                                 (expand-file-name
                                  (format "~/.nvm/versions/node/v%s*/bin"
                                          version))))))))
  (dolist (dir (delq nil (list "~/.local/bin" "~/.pyenv/bin" nvm-bin)))
    (let ((d (expand-file-name dir)))
      (add-to-list 'exec-path d)
      (unless (member d (split-string (or (getenv "PATH") "") ":"))
        (setenv "PATH" (concat d ":" (getenv "PATH")))))))

;; Load custom file
(setq-default custom-file (concat user-emacs-directory "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file))

;; Load my config files
(load (concat user-emacs-directory "utils.el"))
(load (concat user-emacs-directory "eglot-config.el"))
(load (concat user-emacs-directory "org-config.el"))
(load (concat user-emacs-directory "eshell-config.el"))
(load (concat user-emacs-directory "completion-config.el"))
;; BTC price modeline widget — enable with M-x btc-price-mode
(load (concat user-emacs-directory "btc-price.el"))
;; Quick-view dashboard (world-clock, BTC, agenda) — bound to C-c s
(load (concat user-emacs-directory "dashboard.el"))
;; CV exporter — M-x cv-export-to-typst
(load (concat user-emacs-directory "cv-export.el"))

;; Load my settings (settings.el is tangled from settings.org on save)
(load (concat user-emacs-directory "settings.el"))

;; GUI-only configuration (fonts, icons, pixel scrolling, etc.)
;; In daemon mode display-graphic-p is nil at startup, so defer to frame creation.
(add-hook 'after-make-frame-functions
          (lambda (frame)
            (when (and (display-graphic-p frame)
                       (not (featurep 'gui-config)))
              (with-selected-frame frame
                (load (concat user-emacs-directory "gui-config.el"))))))
(unless (daemonp)
  (when (display-graphic-p)
    (load (concat user-emacs-directory "gui-config.el"))))

;; ESC to escape (along with C-g)
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; Reset the working directory regardless of where Emacs was started
(cd "~/")

;; Enable loopback so that pinentry prompts appear inside Emacs.
(use-package pinentry
  :demand t
  :ensure t
  :config
  (setq epg-pinentry-mode 'loopback)
  (pinentry-start))

;; epa-file is built into Emacs — no install needed.
(use-package epa-file
  :ensure nil
  :config
  (epa-file-enable))

;; GPG_TTY is only meaningful in terminal sessions (pinentry needs it for passphrase prompts).
;; Calling `tty` in a GUI frame returns an error string, so guard it.
(unless (display-graphic-p)
  (setenv "GPG_TTY" (string-trim (shell-command-to-string "tty"))))

;; Env variables
(use-package exec-path-from-shell
  :demand t
  :config
  ;; Specify the environment variables ECA needs
  (setq exec-path-from-shell-variables
        '("ANTHROPIC_API_KEY"
          "OPENAI_API_KEY"
          "OLLAMA_API_BASE"
          "OPENAI_API_URL"
          "ANTHROPIC_API_URL"
          "ECA_CONFIG"
          "XDG_CONFIG_HOME"
          "PATH"
          "MANPATH"))
  (setq exec-path-from-shell-debug nil)
  (dolist (var '("SHELL" "SSH_AUTH_SOCK" "SSH_AGENT_PID" "PATH" "HOME" "LSP_USE_PLISTS"))
    (add-to-list 'exec-path-from-shell-variables var))
    ;; Only needed on macOS where GUI Emacs doesn't inherit shell env
  (when (memq window-system '(mac ns))
    (exec-path-from-shell-initialize)))


(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

(provide 'init.el)
;;; init.el ends here
