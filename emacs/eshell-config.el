;;; eshell-config.el --- Eshell configuration  -*- lexical-binding: t -*-
;;;
;;; Commentary:
;;; Eshell settings: history, completion, display, visual commands.
;;; For a full PTY terminal emulator see vterm (C-c v).
;;;
;;; Code:

(use-package eshell
  :ensure nil
  :commands eshell
  :config
  (setq eshell-highlight-prompt t
		eshell-buffer-shorthand t
		eshell-cmpl-ignore-case t
		eshell-history-size 500
		eshell-save-history-on-exit t
		eshell-buffer-maximum-lines 20000
		eshell-hist-ignoredups t
		eshell-last-dir-ring-size 500
		eshell-cmpl-cycle-completions nil
		eshell-destroy-buffer-when-process-dies t
		eshell-visual-commands '("htop" "tail" "less" "more" "top" "vim" "vi"))

  ;; Display Eshell buffer at the bottom of the frame.
  ;; display-buffer-at-bottom does not use side/slot params (those are for
  ;; display-buffer-in-side-window); only window-height is relevant here.
  (add-to-list 'display-buffer-alist
               '("*eshell*" (display-buffer-at-bottom)
                 (window-height . 20))))


;; Use a login shell for shell-command so PATH and functions from ~/.zshrc are available.
;; Avoid -i (interactive): it sources the full shell init on every shell-command call,
;; which adds latency and can produce spurious output (prompts, motd, etc.).
(setq shell-command-switch "-lc")

(provide 'eshell-config)
;;; eshell-config.el ends here
