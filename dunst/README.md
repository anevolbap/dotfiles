# Dunst notification daemon

Replaces `xfce4-notifyd` as the D-Bus notification daemon
(`org.freedesktop.Notifications`).

## Contents

| Path | Purpose |
| --- | --- |
| `.config/dunst/dunstrc` | Dunst configuration |
| `.config/autostart/dunst.desktop` | Starts dunst at graphical login |
| `.config/autostart/xfce4-notifyd.desktop` | `Hidden=true` override that keeps xfce4-notifyd from autostarting |

## Packages

This repo does not manage system packages. Install them once per machine:

```sh
sudo apt update
sudo apt install dunst libnotify-bin
```

`dunst` is the daemon, `libnotify-bin` provides `notify-send`.

## Install

From the dotfiles repo root:

```sh
make dunst
```

That stows the package into `$HOME`, so `~/.config/dunst/dunstrc` and both
autostart entries become symlinks into the repo. Edits in the repo take effect
on the next `dunstctl reload`.

## Disabling xfce4-notifyd

XFCE starts its daemon from `/etc/xdg/autostart/xfce4-notifyd.desktop`. The
system file is left alone. `.config/autostart/xfce4-notifyd.desktop` shadows it
by basename with `Hidden=true`, which the XDG autostart spec defines as "treat
this entry as if it did not exist".

Both daemons also ship a D-Bus activation file
(`/usr/share/dbus-1/services/org.knopwob.dunst.service` and
`org.xfce.xfce4-notifyd.Notifications.service`). Since dunst autostarts at
login and takes the bus name, xfce4-notifyd is never activated.

To revert, run `make dunst` after deleting the override, or just
`rm ~/.config/autostart/xfce4-notifyd.desktop`.

## Verify

```sh
pgrep -a dunst          # should print a dunst process
pgrep -a xfce4-notifyd  # should print nothing
```

```sh
notify-send "Dunst test" "Hello from my notification daemon"
notify-send -u low "Low priority" "Disappears after 3s"
notify-send -u normal "Normal priority" "Disappears after 5s"
notify-send -u critical "Critical" "Stays until dismissed"
notify-send -i dialog-information "Icon" "Resolved from the Adwaita theme"
notify-send -h int:value:65 -i audio-volume-high "Volume" "Progress bar at 65%"
dunstctl count
dunstctl history
dunstctl is-paused
```

Repeats collapse: send the same summary and body three times and
`dunstctl count` should show one notification with a counter, not three.

## Emacs and shell integration

Emacs talks to dunst through the built-in `notifications.el`, no package
needed. `emacs/utils.el` wraps it:

```elisp
(ao/notify "Title" "Body" 'critical)   ; urgency defaults to 'normal
```

The body is escaped, because `markup = full` means dunst parses it as Pango
markup and a raw `<` would break the notification.

`compilation-finish-functions` uses that helper, so `M-x compile` and
`recompile` notify when they end: normal urgency on success, critical on
failure, with the elapsed time in the body. Runs shorter than
`ao/notify-compilation-threshold` (10 seconds) stay silent, which keeps
`M-x grep` from popping a notification every time.

Org needs nothing: `org-show-notification` in `org-clock.el` already calls
`notifications-notify`, so `org-timer-set-timer` and clock reminders go
through dunst as they are.

Outside Emacs, `local/.local/bin/notify-when-done` (installed by `make local`)
runs a command and notifies when it ends, passing the exit status through:

```sh
notify-when-done pytest
notify-when-done make -j8
```

## Two rules worth knowing

`[emacs-icon]` sets the icon for anything with appname `Emacs`. Emacs ships
its icon only in `hicolor`, and dunst 1.9.0 stops at the first `hicolor`
theme it finds, which here is flatpak's empty one at
`/var/lib/flatpak/exports/share/icons/hicolor`. So `/usr/share/icons/hicolor`
is never searched and lookup by name fails. The rule points at the file
instead. Any other app whose icon lives only in hicolor needs the same
treatment. Adwaita names such as `dialog-information` are unaffected.

`[corne-alert-critical]` runs `~/.local/bin/corne-rgb-alert` on critical
notifications, so a failed test run blinks the keyboard. dunst expands the
`~` and passes five arguments the script ignores. Nothing happens when the
Corne is not plugged in.

## Keyboard shortcuts

Dunst 1.9 has no built-in keybindings. Bind these through the window manager
(XFCE: Settings, Keyboard, Application Shortcuts):

```sh
dunstctl close             # dismiss the top notification
dunstctl close-all         # dismiss everything on screen
dunstctl history-pop       # bring back the last dismissed one
dunstctl context           # action menu for the top notification
dunstctl set-paused toggle # mute and unmute notifications
```

Right click and `dunstctl context` need `dmenu` to pick between actions:

```sh
sudo apt install suckless-tools
```

Without it, left click still runs a notification's default action.

## Notes

- Config validated against dunst 1.9.0 (Debian 12). Check for bad settings
  with:

  ```sh
  dunst -conf ~/.config/dunst/dunstrc -verbosity warn
  ```

  Run it while dunst is already up: it prints any bad setting, then exits
  because it cannot take the bus name. `-verbosity info` also shows which
  icon themes loaded.

- `frame_width` is a global-only setting in this version, so
  `[urgency_critical]` uses `frame_color` for emphasis instead.

- Icons are resolved by name from the Adwaita theme
  (`enable_recursive_icon_lookup`), which makes `icon_path` inert. The XFCE
  desktop theme is Tango, but Adwaita covers notification icon names better.

- Colours are Nord. Low urgency is dimmed, critical gets a red frame and a
  warm background.

- `idle_threshold = 120` holds notifications instead of expiring them once
  the screen has been idle two minutes, so nothing is missed while away.

- Transient notifications (volume and brightness popups) are kept out of
  history by the `transient-history-ignore` rule.

- Nothing here depends on XFCE except the autostart mechanism. Under i3 or
  EXWM, keep `dunstrc` stowed and start dunst from the window manager's own
  startup (or drop the autostart entries and rely on D-Bus activation).
