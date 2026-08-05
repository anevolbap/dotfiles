;; early-init.el --- Early startup configuration  -*- lexical-binding: t -*-

;; Disable package.el — elpaca manages packages instead.
(setq package-enable-at-startup nil)

;; Raise GC threshold during startup to reduce GC pauses.
;; Reset to a reasonable value afterwards so normal use isn't penalised.
;; http://bling.github.io/blog/2016/01/18/why-are-you-changing-gc-cons-threshold/
(setq gc-cons-threshold (* 50 1000 1000))
;; Reset after elpaca finishes processing its queue, not at after-init-hook
;; (which fires before elpaca is done and would lower the threshold mid-install).
(add-hook 'elpaca-after-init-hook
          (lambda () (setq gc-cons-threshold (* 2 1000 1000))))

;; (setq garbage-collection-messages t)

;; Set early so it is in effect during package loading, not just after.
(setq read-process-output-max (* 1024 1024)) ; 1 MB — improves LSP throughput

;; *scratch* defaults to `lisp-interaction-mode', which derives from `prog-mode',
;; so every `prog-mode-hook' entry (highlight-indent-guides, hideshow, hl-line,
;; line numbers, buffer fonts) loads during startup.
(setq initial-major-mode 'fundamental-mode)

;; Local Variables:
;; no-byte-compile: t
;; no-native-compile: t
;; no-update-autoloads: t
;; End:
