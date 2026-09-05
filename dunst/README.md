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
dunstctl history
dunstctl is-paused
```

## Notes

- Config validated against dunst 1.9.0 (Debian 12). `frame_width` is a
  global-only setting in this version, so `[urgency_critical]` uses
  `frame_color` for emphasis instead. Check for parse errors with:

  ```sh
  dunst -conf ~/.config/dunst/dunstrc -verbosity warn
  ```

  Run it while dunst is already up: it prints any bad setting, then exits
  because it cannot take the bus name.

- Nothing here depends on XFCE except the autostart mechanism. Under i3 or
  EXWM, keep `dunstrc` stowed and start dunst from the window manager's own
  startup (or drop the autostart entries and rely on D-Bus activation).
