;;; -*- lexical-binding: t; -*-
;;; gui-config.el --- GUI-only Emacs configuration
;;; Loaded only when Emacs is running in graphical mode.
;;; See init.el: (when (display-graphic-p) (load "gui-config.el"))
;;;
;;; Code:

;; ============================================================================
;; Scrolling
;; ============================================================================

(pixel-scroll-precision-mode)

;; ============================================================================
;; Cursor
;; ============================================================================

(setq-default cursor-type 'box)

;; ============================================================================
;; Tooltips
;; Show tooltip text in the echo area instead of a separate frame. The
;; tooltip frame on this Emacs+GTK3 build triggers a GdkX11Window->GtkWindow
;; cast warning whenever it is resized.
;; ============================================================================

(setq tooltip-use-echo-area t)

;; ============================================================================
;; Fringe and window divider pixel faces
;; ============================================================================

(set-face-attribute 'fringe nil :background "black")
(set-face-attribute 'window-divider-first-pixel nil :foreground "gray40")
(set-face-attribute 'window-divider-last-pixel nil :foreground "red")

;; Slightly shorter mode-line to reclaim vertical space.
(set-face-attribute 'mode-line          nil :height 0.85)
(set-face-attribute 'mode-line-inactive nil :height 0.85)

;; Let markdown code blocks use the buffer's default (variable-pitch) font
;; instead of forcing monospace — org-mode/markdown bodies read better that way.
(with-eval-after-load 'markdown-mode
  (set-face-attribute 'markdown-code-face nil :inherit 'default))

;; ============================================================================
;; macOS option key
;; ============================================================================

(when (eq system-type 'darwin)
  (setq mac-right-option-modifier 'none))

;; ============================================================================
;; Fonts (Iosevka)
;; https://github.com/be5invis/Iosevka
;; ============================================================================

(defun ao/buffer-face-mode-variable ()
  "Set font to a variable width (proportional) font in current buffer."
  (interactive)
  (setq buffer-face-mode-face '(:family "Iosevka" :height 140 :width semi-condensed))
  (buffer-face-mode))

(defun ao/buffer-face-mode-fixed ()
  "Set font to a fixed width (monospace) font in current buffer."
  (interactive)
  (setq buffer-face-mode-face '(:family "Iosevka" :height 140))
  (buffer-face-mode))

(add-hook 'prog-mode-hook 'ao/buffer-face-mode-fixed)
(add-hook 'yaml-ts-mode-hook 'ao/buffer-face-mode-fixed)
(add-hook 'org-mode-hook 'ao/buffer-face-mode-variable)
(add-hook 'text-mode-hook 'ao/buffer-face-mode-variable)
(add-hook 'eww-mode-hook 'ao/buffer-face-mode-variable)
(add-hook 'elfeed-show-mode-hook 'ao/buffer-face-mode-variable)

;; ============================================================================
;; Icons (requires Nerd Font installed and set in the terminal/GUI)
;; nerd-icons packages are loaded in settings.el

(provide 'gui-config)
;;; gui-config.el ends here
