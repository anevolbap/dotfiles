(defun org-babel-tangle-config ()
(when (string-equal (file-name-directory (buffer-file-name))
(expand-file-name "~/.emacs.d/"))
;; Dynamic scoping to the rescue
(let ((org-confirm-babel-evaluate nil))
(org-babel-tangle))))

(add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'org-babel-tangle-config)))

;; Automatically update buffers if file content on the disk has changed.
(global-auto-revert-mode t)
;; save session          
(desktop-save-mode 0)    
;; battery               
(display-battery-mode 1) 
(display-time-mode 1)

;;(setq-default cursor-type 'box)  
(setq display-time-day-and-date t)

;; Line spacing                                   
(setq-default line-spacing 3)                     
;; No cursor in non-selected windows              
(setq-default cursor-in-non-selected-windows nil)

(setq diff-switches "-u")

(put 'narrow-to-region 'disabled nil)

(setq dotfiles-dir (expand-file-name "~/.emacs.d/"))
(setq-default custom-file (concat dotfiles-dir "custom.el"))
(when (file-exists-p custom-file)
(load custom-file))

;; dotfiles
(setq dotfiles-dir (expand-file-name "~/.emacs.d/"))
(add-to-list 'load-path (concat dotfiles-dir "lisp/"))
(add-to-list 'load-path (concat dotfiles-dir "elpa/"))

(use-package auto-package-update
:custom
(auto-package-update-interval 7)
(auto-package-update-prompt-before-update t)
(auto-package-update-hide-results t)
:config
(auto-package-update-maybe)
(auto-package-update-at-time "09:00"))

(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(setq default-buffer-file-coding-system 'utf-8)

(global-font-lock-mode t)

(use-package term
:config
(setq explicit-shell-file-name "bash") ;; Change this to zsh, etc
;;(setq explicit-zsh-args '())         ;; Use 'explicit-<shell>-args for shell-specific args

;; Match the default Bash shell prompt.  Update this if you have a custom prompt
(setq term-prompt-regexp "^[^#$%>\n]*[#$%>] *"))

(use-package eterm-256color
:hook (term-mode . eterm-256color-mode))

(use-package vterm
:commands vterm
:config
(setq term-prompt-regexp "^[^#$%>\n]*[#$%>] *")  ;; Set this to match your custom shell prompt
;;(setq vterm-shell "zsh")                       ;; Set this to customize the shell to launch
(setq vterm-max-scrollback 10000))

(use-package eshell-git-prompt)

(use-package eshell
:hook (eshell-first-time-mode . configure-eshell)
:config

(with-eval-after-load 'esh-opt
(setq eshell-destroy-buffer-when-process-dies t)
(setq eshell-visual-commands '("htop" "zsh" "vim")))

(eshell-git-prompt-use-theme 'powerline))

(use-package all-the-icons)

(use-package all-the-icons-dired               
:hook (dired-mode . all-the-icons-dired))

(use-package minions
:hook (doom-modeline-mode . minions-mode)
:custom
(minions-mode-line-lighter ""))

(use-package doom-modeline
:after eshell     ;; Make sure it gets hooked after eshell
:hook (after-init . doom-modeline-init)
:custom-face
(mode-line ((t (:height 0.85))))
(mode-line-inactive ((t (:height 0.85))))
:custom
(doom-modeline-height 15)
(doom-modeline-bar-width 6)
(doom-modeline-lsp t)
(doom-modeline-github nil)
(doom-modeline-mu4e nil)
(doom-modeline-irc nil)
(doom-modeline-minor-modes t)
(doom-modeline-persp-name nil)
(doom-modeline-buffer-file-name-style 'truncate-except-project)
(doom-modeline-major-mode-icon nil))

(setq initial-scratch-message "")
(setq inhibit-startup-message t) ;; no intro

(scroll-bar-mode -1) ;; disable horizontal scrollbar
(menu-bar-mode -1) ;; disable the menubar
(tool-bar-mode -1) ;; gets rid of the tool bar at the top.
(tooltip-mode -1) ;; disable tooltips
(column-number-mode t) ;; Display column number in mode line.
(global-visual-line-mode t) ;; Wrap words
(set-fringe-mode 10) ;; bordecito
(fset 'yes-or-no-p 'y-or-n-p) ;; Change all yes/no questions to y/n type

;; Set up the visible bell
(setq visible-bell t)

(column-number-mode)
(global-display-line-numbers-mode t)

  ;; Disable line numbers for some modes
  ;;          (dolist (mode '(
  ;;          org-mode-hook 
  ;;          shell-mode-hook
  ;;          eshell-mode-hook)
  ;;          (add-hook mode (lambda () (display-line-numbers-mode 0)))))

(setq make-pointer-invisible t)

(setq-default fill-column 80)
(setq-default default-tab-width 2)
(setq-default indent-tabs-mode nil)

;; (global-set-key (kbd "C-w +") 'shrink-window-horizontally)
;; (global-set-key (kbd "C-w -") 'enlarge-window-horizontally)

(winner-mode 1)
(global-set-key (kbd "<C-c-right>") 'winner-redo)
(global-set-key (kbd "<C-c-left>") 'winner-undo)

(load-theme 'modus-vivendi t)

(add-hook 'text-mode-hook 'auto-fill-mode)

;; Do tabs right
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq indent-line-function 'insert-tab)
(setq c-basic-offset 4)

(show-paren-mode 1)

(use-package rainbow-delimiters 
:hook (prog-mode-hook . rainbow-delimiters-mode))

(set-face-attribute 'default nil :font "Roboto Mono" :height 100)
;; Set the fixed pitch face
(set-face-attribute 'fixed-pitch nil :font "Roboto Mono" :height 100)
; Set the variable pitch face
(set-face-attribute 'variable-pitch nil :font "Roboto Mono"
:height 100 :weight 'regular)

(cua-mode t)

(delete-selection-mode 1)

(use-package aggressive-indent
:hook
(css-mode . aggressive-indent-mode)
(emacs-lisp-mode . aggressive-indent-mode)
(js-mode . aggressive-indent-mode)
(lisp-mode . aggressive-indent-mode)
(sgml-mode . aggressive-indent-mode)
:custom
(aggressive-indent-comments-too t)
:config
(add-to-list 'aggressive-indent-protected-commands 'comment-dwim))

(use-package highlight-indent-guides
:hook
(python-mode . highlight-indent-guides-mode)
(scss-mode . highlight-indent-guides-mode)
:custom
(highlight-indent-guides-method 'character))

(use-package hl-line
:ensure nil
:hook
(prog-mode . hl-line-mode)
(special-mode . hl-line-mode)
(text-mode . hl-line-mode))

(use-package whitespace
:ensure nil
:hook
(prog-mode . whitespace-mode)
(text-mode . whitespace-mode)
:custom
(whitespace-style '(face empty indentation::space tab trailing)))

(setq diff-switches "-u")

(use-package vc
:config
(setq vc-follow-symlinks t)) ; because dotfiles are managed with stow

(use-package magit
:after vc
:defer t
:bind (("C-x g" . magit-status))
)
; require is only so we can remove the vc hook:
(remove-hook 'find-file-hooks 'vc-find-file-hook)

(use-package dired
 :ensure nil
 :defer 1
 :commands (dired dired-jump)
 :bind (("C-x C-j" . dired-jump))
 :config
 (setq dired-listing-switches "-agho --group-directories-first"
 dired-omit-files "^\\.[^.].*"
 dired-omit-verbose nil)
 )

 (use-package dired-ranger
 :defer t)

 (use-package dired-collapse
 :defer t)

 (use-package dired-single
 :defer t)

;; Make dired less verbose                            
;;   (require 'dired-details)                        
;;   (setq-default dired-details-hidden-string "---")     
;;   (dired-details-install)

(use-package projectile
:diminish projectile-mode
:config (projectile-mode)
:custom ((projectile-completion-system 'ivy))
:bind-keymap
("C-c p" . projectile-command-map)
:init
;; NOTE: Set this to the folder where you keep your Git repos!
(when (file-directory-p "~/Documents/repositorios")
(setq projectile-project-search-path '("~/Documents/repositorios")))
(setq projectile-switch-project-action #'projectile-dired))

(use-package counsel-projectile
:config (counsel-projectile-mode))

(global-set-key (kbd "C-x w") 'elfeed)
(setq elfeed-feeds
'(("https://www.reddit.com/r/emacs/comments.rss" emacs)
("https://www.infobae.com/feeds/rss/" infobae)
("https://www.r-users.com/feed/?post_type=job_listing" rjobs)
("https://www.reddit.com/r/emacs.rss" emacs)))

(use-package eradio
:defer t
:config
(setq eradio-player '("quodlibet"))
(setq eradio-channels '(
;; electronica with defcon-speaker bumpers
("def con - soma fm" . "https://somafm.com/defcon256.pls") 
;; \m/
("metal - soma fm"   . "https://somafm.com/metal130.pls")          
;; cyberpunk-esque electronica
("cyberia - lainon"  . "https://lainon.life/radio/cyberia.ogg.m3u") 
;; boring ambient, but with lain
("cafe - lainon"     . "https://lainon.life/radio/cafe.ogg.m3u"))))  
(global-set-key (kbd "C-c r p") 'eradio-play)
(global-set-key (kbd "C-c r s") 'eradio-stop)

(setq browse-url-browser-function
'(
("meet.google.com" . browse-url-chrome)
("thefreedictionary\\.com" . eww-browse-url)
("." . browse-url-default-browser)
))

(use-package google-this
:defer t
:diminish t
:config (google-this-mode 1)
)

(use-package google-translate
:defer t
:commands (google-translate-query-translate-reverse
google-translate-query-translate
google-translate-at-point
google-translate-at-point-reverse)
;; :init
;; (progn
;; (bind-key "C-g l" 'google-translate-query-translate-reverse)
;; (bind-key "C-g L" 'google-translate-query-translate)
;; (bind-key "C-g K" 'google-translate-at-point)
;; (bind-key "C-g k" 'google-translate-at-point-reverse)
;; )
:config
(setq google-translate-default-source-language "en")
(setq google-translate-default-target-language "ru")
(org-babel-load-file "~/.emacs.d/init-google-translate.org") 
)

(use-package treemacs
:ensure t
:defer t
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :config
  (progn
    (setq treemacs-collapse-dirs                 (if treemacs-python-executable 3 0)
          treemacs-deferred-git-apply-delay      0.5
          treemacs-directory-name-transformer    #'identity
          treemacs-display-in-side-window        t
          treemacs-eldoc-display                 t
          treemacs-file-event-delay              5000
          treemacs-file-extension-regex          treemacs-last-period-regex-value
          treemacs-file-follow-delay             0.2
          treemacs-file-name-transformer         #'identity
          treemacs-follow-after-init             t
          treemacs-git-command-pipe              ""
          treemacs-goto-tag-strategy             'refetch-index
          treemacs-indentation                   2
          treemacs-indentation-string            " "
          treemacs-is-never-other-window         nil
          treemacs-max-git-entries               5000
          treemacs-missing-project-action        'ask
          treemacs-move-forward-on-expand        nil
          treemacs-no-png-images                 nil
          treemacs-no-delete-other-windows       t
          treemacs-project-follow-cleanup        nil
          treemacs-persist-file                  (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
          treemacs-position                      'left
          treemacs-read-string-input             'from-child-frame
          treemacs-recenter-distance             0.1
          treemacs-recenter-after-file-follow    nil
          treemacs-recenter-after-tag-follow     nil
          treemacs-recenter-after-project-jump   'always
          treemacs-recenter-after-project-expand 'on-distance
          treemacs-show-cursor                   nil
          treemacs-show-hidden-files             t
          treemacs-silent-filewatch              nil
          treemacs-silent-refresh                nil
          treemacs-sorting                       'alphabetic-asc
          treemacs-space-between-root-nodes      t
          treemacs-tag-follow-cleanup            t
          treemacs-tag-follow-delay              1.5
          treemacs-user-mode-line-format         nil
          treemacs-user-header-line-format       nil
          treemacs-width                         35
          treemacs-workspace-switch-cleanup      nil)

    ;; The default width and height of the icons is 22 pixels. If you are
    ;; using a Hi-DPI display, uncomment this to double the icon size.
    (treemacs-resize-icons 44)

    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode 'always)
    (pcase (cons (not (null (executable-find "git")))
                 (not (null treemacs-python-executable)))
      (`(t . t)
       (treemacs-git-mode 'deferred))
      (`(t . _)
       (treemacs-git-mode 'simple))))
  :bind
  (:map global-map
        ("M-0"       . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t t"   . treemacs)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag)))

(use-package treemacs-projectile
  :after treemacs projectile
  :ensure t)

(use-package treemacs-icons-dired
  :after treemacs dired
  :ensure t
  :config (treemacs-icons-dired-mode))

(use-package treemacs-magit
  :after treemacs magit
  :ensure t)

(use-package treemacs-persp ;;treemacs-perspective if you use perspective.el vs. persp-mode
  :after treemacs persp-mode ;;or perspective vs. persp-mode
  :ensure t
  :config (treemacs-set-scope-type 'Perspectives))

(use-package evil-nerd-commenter
:bind ("M-/" . evilnc-comment-or-uncomment-lines))

;;(defun lsp-mode-setup ()
;;(setq lsp-headerline-breadcrumb-segments '(path-up-to-project file symbols))
;;(lsp-headerline-breadcrumb-mode))

(use-package lsp-mode
:init
(setq lsp-keymap-prefix "C-c l")
:hook (
(python-mode . lsp)
(lsp-mode . lsp-enable-which-key-integration))
:commands (lsp lsp-deferred))

(use-package lsp-jedi
:ensure t
:config
(with-eval-after-load "lsp-mode"
(add-to-list 'lsp-disabled-clients 'pyls)
(add-to-list 'lsp-enabled-clients 'jedi)))

(use-package lsp-ui
:ensure t
:config
(setq lsp-ui-sideline-ignore-duplicate t)
(add-hook 'lsp-mode-hook 'lsp-ui-mode)
)
;;      :hook (lsp-mode . lsp-ui-mode)
;; :custom 
;;(lsp-ui-doc-position 'bottom)
;      (setq lsp-ui-sideline-enable t)
;     (setq lsp-ui-sideline-show-hover nil)
;      (lsp-ui-doc-show)


;;(use-package lsp-treemacs
;;:config (lsp-treemacs-sync-mode 1)
;;:after lsp)

(use-package lsp-ivy 
:commands lsp-ivy-workspace-symbol)

(use-package python-mode
;; :ensure nil
:hook (python-mode . lsp-deferred)
  :custom
  ;; (py-shell-name "python3")
  (python-shell-interpreter "python3")
  (dap-python-executable "python3")
  (dap-python-debugger 'debugpy)
  :config
  (require 'dap-python))

  (use-package pyvenv
  :config
  (pyvenv-mode 1)
  )

  ;; (use-package elpy
  ;; :init (setq python-shell-interpreter "python3")
  ;; :defer t
  ;; :ensure t
  ;; :after python
  ;; :config (elpy-enable))

;; (use-package virtualenvwrapper
;; :defer t
;; :config
;; (venv-initialize-interactive-shells)
;; (venv-initialize-eshell) ;; if you want eshell support
;; (setq venv-location (expand-file-name "~/.virtualenvs/"))
;; (add-hook 'python-mode-hook (lambda ()
;; (hack-local-variables)
;; (venv-workon project-venv-name))))

;; Output PDF
(setq TeX-view-program-list '(("xournal" "/usr/bin/xournal %o")))
(setq TeX-view-program-selection '((output-pdf "xournal")))

;; (setq TeX-parse-self t)
;; (setq reftex-plug-into-AUCTeX t)
;; (setq-default TeX-master nil)
;; (setq TeX-PDF-mode t)
(require 'company-auctex)
;; refresh del pdf en emacs
(add-hook 'TeX-after-compilation-finished-functions #'TeX-revert-document-buffer)

(add-hook 'LaTeX-mode-hook 'visual-line-mode)
(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
(add-hook 'LaTeX-mode-hook 'TeX-source-correlate-mode)
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)

(use-package markdown-mode
:defer t
:ensure markdown-mode
:commands (markdown-mode gfm-mode)
:mode (("\\.md\\'" . gfm-mode)
       ("\\.Rmd\\'" . markdown-mode)
       ("\\.markdown\\'" . markdown-mode))
       :config (setq markdown-command "pandoc"))

(use-package ess 
:defer t
:config (setq ess-pdf-viewer-pref "xournal")
:mode (("\\.[rR]\\'" . R-mode)
("\\.Rnw\\'" . Rnw-mode)))

(use-package yasnippet
:defer t
:diminish yas-minor-mode
:config (yas-global-mode t))

(use-package dap-mode
;; Uncomment the config below if you want all UI panes to be hidden by
;; default!  :custom (lsp-enable-dap-auto-configure nil) :config
;; (dap-ui-mode 1)
)

(use-package which-key
:init (which-key-mode)
:diminish which-key-mode
:config
(setq which-key-idle-delay 0.3))

(use-package format-all
:bind ("C-f" . format-all-buffer))

;;FIXME: mover a otra seccion
;;ispell config
(setq ispell-program-name "aspell")
(setq ispell-program-name "hunspell")
(setq ispell-list-command "list")
(setq ispell-local-dictionary "es_ES") ;; Change dictionaries here!
(setq ispell-local-dictionary-alist
'(("es_ES" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil nil nil utf-8)))

;; (use-package flyspell            
;; :defer t                         
;; :diminish (flyspell-mode . " φ"))

(remove-hook 'flymake-diagnostic-functions 'flymake-proc-legacy-flymake)

(use-package flycheck
:defer 2
:diminish
:init (global-flycheck-mode)
:hook (lsp-mode . flycheck-mode)
:custom (flycheck-idle-change-delay 0.1)
)

(use-package ivy
:defer 0.1
:diminish
:bind (
("C-s" . swiper)
:map ivy-minibuffer-map
("TAB" . ivy-alt-done)	
("C-l" . ivy-alt-done)
("C-j" . ivy-next-line)
("C-k" . ivy-previous-line)
:map ivy-switch-buffer-map
("C-k" . ivy-previous-line)
("C-l" . ivy-done)
("C-d" . ivy-switch-buffer-kill)
:map ivy-reverse-i-search-map
("C-k" . ivy-previous-line)
("C-d" . ivy-reverse-i-search-kill))
:custom 
(ivy-height 7)
(ivy-count-format "(%d/%d) ")
(ivy-use-virtual-buffers t)
:config (ivy-mode)
)

(use-package ivy-rich
:after ivy
:init (ivy-rich-mode)
)

(use-package counsel
:after ivy
:bind (
("C-M-j" . 'counsel-switch-buffer)
:map minibuffer-local-map
("C-r" . 'counsel-minibuffer-history)
)
:custom
(counsel-linux-app-format-function #'counsel-linux-app-format-function-name-only)
:config (counsel-mode)
)

(use-package company
:after lsp-mode
:hook (lsp-mode . company-mode)
:ensure t
:bind (:map company-active-map
("<tab>" . company-complete-selection))
(:map lsp-mode-map
("<tab>" . company-indent-or-complete-common))
:custom
;; Provide instant autocompletion.
(company-idle-delay 0.0)
(company-show-numbers t)
(company-minimum-prefix-length 2)
(company-tooltip-flip-when-above t))

(use-package company-box
:hook (company-mode . company-box-mode))

(use-package company-lsp
:config
(push 'company-lsp company-backends))

(use-package ivy-prescient
:after counsel
:config
(ivy-prescient-mode 1))

(require 'org)
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-cb" 'org-switchb)
;; (setq load file: No such file or directory, popup t)
;; (setq org-default-notes-file (concat org-directory "/notes.org"))
;; (setq define-key global-map "\C-cc" 'org-capture)
;; (setq org-default-notes-file "/home/pablinha/Dropbox/.notes.org")
;; (setq org-directory "/home/pablinha/Documentos/apps/org")
;; (setq org-support-shift-select t)
(add-hook 'org-mode-hook 'org-hide-block-all)

(defun org-mode-visual-fill ()
(setq visual-fill-column-width 100
visual-fill-column-center-text t)
(visual-fill-column-mode 1))

(setq org-indent-mode t)

(use-package visual-fill-column
:init (add-hook 'org-mode-hook 'org-mode-visual-fill)
;; :hook (org-mode . org-mode-visual-fill)
)

(org-babel-do-load-languages                         
'org-babel-load-languages                            
'((emacs-lisp . t)                                   
(python . t)))                                       

(push '("conf-unix" . conf-unix) org-src-lang-modes)

(use-package org-bullets                                  
:after org                                                
:init
(add-hook 'org-mode-hook 'org-bullets-mode)
(setq org-bullets-bullet-list '("◉" "○" "●" "►" "•")))
