(defun efs/run-in-background (command)
 (let ((command-parts (split-string command "[ ]+")))
 (apply #'call-process `(,(car command-parts) nil 0 nil ,@(cdr command-parts)))))

 (defun efs/set-wallpaper ()
 (interactive)
 ;; NOTE: You will need to update this to a valid background path!
 (start-process-shell-command
 "feh" nil  "feh --bg-scale /home/pablo/Pictures/prueba.jpg"))

 (defun efs/exwm-init-hook ()
 ;; Make workspace 1 be the one where we land at startup
 (exwm-workspace-switch-create 1)

 ;; Open eshell by default
 ;;(eshell)

 ;; Show battery status in the mode line
(display-battery-mode 1)

;; Show the time and date in modeline
(setq display-time-day-and-date t)
(display-time-mode 1)
;; Also take a look at display-time-format and format-time-string

;; Launch apps that will run in the background
(efs/run-in-background "nm-applet")
(efs/run-in-background "pasystray")
;; (efs/run-in-background "blueman-applet")
)

(defun efs/exwm-update-class ()
(exwm-workspace-rename-buffer exwm-class-name))

(defun efs/exwm-update-title ()
(pcase exwm-class-name
("Firefox" (exwm-workspace-rename-buffer (format "Firefox: %s" exwm-title)))))

(defun efs/configure-window-by-class ()
(interactive)
(pcase exwm-class-name
("Firefox" (exwm-workspace-move-window 2))
("Sol" (exwm-workspace-move-window 3))
("mpv" (exwm-floating-toggle-floating)
(exwm-layout-toggle-mode-line))))

(use-package exwm
:config
;; Set the default number of workspaces
(setq exwm-workspace-number 5)

;; When window "class" updates, use it to set the buffer name
(add-hook 'exwm-update-class-hook #'efs/exwm-update-class)

;; When window title updates, use it to set the buffer name
(add-hook 'exwm-update-title-hook #'efs/exwm-update-title)

;; Configure windows as they're created
(add-hook 'exwm-manage-finish-hook #'efs/configure-window-by-class)

;; When EXWM starts up, do some extra confifuration
(add-hook 'exwm-init-hook #'efs/exwm-init-hook)

;;(exwm-input-set-key (kbd "s-SPC") 'counsel-linux-app)
;;(exwm-input-set-key (kbd "s-f") 'exwm-layout-toggle-fullscreen)

(require 'exwm-randr)
(exwm-randr-enable)
(start-process-shell-command "xrandr" nil "xrandr --output LVDS-1 --auto --primary --output HDMI-1 --mode 1920x1080 --above LVDS-1")

 (setq exwm-randr-workspace-monitor-plist '(2 "LVDS-1"))

 (require 'exwm-systemtray)
 (add-hook 'exwm-randr-screen-change-hook #'efs/update-displays)
 (efs/update-displays)

 (setq exwm-systemtray-height 32)
 (exwm-systemtray-enable)

 ;; Automatically send the mouse cursor to the selected workspace's display
 (setq exwm-workspace-warp-cursor t)

 ;; Window focus should follow the mouse pointer
 (setq mouse-autoselect-window t
      focus-follows-mouse t)


 ;; These keys should always pass through to Emacs
 (setq exwm-input-prefix-keys
 '(?\C-x
 ?\C-u
 ?\C-h
 ?\M-x
 ?\M-`
 ?\M-&
 ?\M-:
 ?\C-\M-j  ;; Buffer list
 ?\C-\ ))  ;; Ctrl+Space

 ;; Ctrl+Q will enable the next key to be sent directly
 (define-key exwm-mode-map [?\C-q] 'exwm-input-send-next-key)

 (add-hook 'exwm-init-hook #'efs/after-exwm-init)
 ;; Set up global key bindings.  These always work, no matter the input state!
 ;; Keep in mind that changing this list after EXWM initializes has no effect.
 (setq exwm-input-global-keys
 `(
 ;; Reset to line-mode (C-c C-k switches to char-mode via exwm-input-release-keyboard)
 ([?\s-r] . exwm-reset)

 ;; Move between windows
 ([s-left] . windmove-left)
 ([s-right] . windmove-right)
 ([s-up] . windmove-up)
 ([s-down] . windmove-down)

 ;; Launch applications via shell command
 ([?\s-&] . (lambda (command)
 (interactive (list (read-shell-command "$ ")))
 (start-process-shell-command command nil command)))

 ;; Switch workspace
 ([?\s-w] . exwm-workspace-switch)

 ;; 's-N': Switch to certain workspace with Super (Win) plus a number key (0 - 9)
 ,@(mapcar (lambda (i)
 `(,(kbd (format "s-%d" i)) .
 (lambda ()
 (interactive)
 (exwm-workspace-switch-create ,i))))
 (number-sequence 0 9))))

 (exwm-enable)
 )

 (use-package desktop-environment
 :after exwm
 :config (desktop-environment-mode)
 :custom
 (desktop-environment-brightness-small-increment "2%+")
 (desktop-environment-brightness-small-decrement "2%-")
 (desktop-environment-brightness-normal-increment "5%+")
 (desktop-environment-brightness-normal-decrement "5%-")
 )

 (defun efs/run-in-background (command)
 (let ((command-parts (split-string command "[ ]+")))
 (apply #'call-process `(,(car command-parts) nil 0 nil ,@(cdr command-parts)))))

 (defun dw/exwm-init-hook ()
 ;; Make workspace 1 be the one where we land at startup
 (exwm-workspace-switch-create 1)

 ;; Open eshell by default
 ;;(eshell)

 ;; Launch apps that will run in the background
 (efs/run-in-background "nm-applet"))
