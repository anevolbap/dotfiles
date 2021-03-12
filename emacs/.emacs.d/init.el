;;; init.el  --- My emacs configuration file

;;; Code:
(let ((default-directory user-emacs-directory)
      (file-name-handler-alist nil)
      (gc-cons-percentage .6)
      (gc-cons-threshold most-positive-fixnum)
      (read-process-output-max (* 1024 1024)))

  ;; Disable that pesky echo message
  (setq-default inhibit-startup-echo-area-message (user-login-name))

;; Repositories
(require 'package)
(setq-default load-prefer-newer t package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/"))
(add-to-list 'package-archives '("elpa" . "https://elpa.gnu.org/packages/"))
(package-initialize)
(unless package-archive-contents package-refresh-contents))

;; Load use-package, used for loading packages
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package t))
(setq use-package-always-ensure t
      use-package-always-defer t)

;; Configure `use-package' prior to loading it.
(eval-and-compile
  (setq use-package-always-demand nil)
  (setq use-package-expand-minimally nil)
  (setq use-package-enable-imenu-support t)
  (setq use-package-compute-statistics t)
  ;; The following is VERY IMPORTANT.  Write hooks using their real name
  ;; instead of a shorter version: after-init ==> `after-init-hook'.
  ;;
  ;; This is to empower help commands with their contextual awareness,
  ;; such as `describe-symbol'.
  (setq use-package-hook-name-suffix nil))

(eval-when-compile
  (require 'use-package))

(use-package benchmark-init
  :ensure t
  :config
  ;; To disable collection of benchmark data after init is done.
  (add-hook 'after-init-hook 'benchmark-init/deactivate))

;; Keep track of loading time
(defconst emacs-start-time (current-time))
(let ((elapsed (float-time (time-subtract (current-time)
                                          emacs-start-time))))
  (message "Loaded packages in %.3fs" elapsed))

;; Load Org
(use-package org
  :defer t)
(org-babel-load-file (expand-file-name "settings.org" user-emacs-directory))
(garbage-collect)

;; Message how long it took to load everything (minus packages)
(let ((elapsed (float-time (time-subtract (current-time)
                                          emacs-start-time))))
  (message "Loading settings...done (%.3fs)" elapsed))

;; Reset the working directory regardless of where Emacs was started
(cd "~/")

;;; init.el ends here
