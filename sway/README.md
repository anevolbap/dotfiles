# Sway

Sway session, installed next to XFCE. LightDM lists both; pick one at login.

## Contents

| Path                  | Purpose                                                                            |
|-----------------------|------------------------------------------------------------------------------------|
| `.config/sway/config` | Copy of `/etc/sway/config` (sway 1.10.1-2) with a "Local changes" block at the end |

The local block sets touchpad tap, starts dunst, nm-applet and the polkit
agent, locks with swayidle and swaylock, and adds region screenshot and media
keys. It also includes `~/.config/sway/config.d/*`, for per-machine settings
such as the keyboard layout:

```
input type:keyboard {
    xkb_layout us
}
```

## Packages

This repo does not manage system packages. Install them once per machine:

```sh
sudo apt install sway xwayland swaylock swayidle xdg-desktop-portal-wlr \
    brightnessctl playerctl slurp grim wl-clipboard
```

`xwayland` is required for X-only programs, including an Emacs built without
`--with-pgtk`.

## Install

```sh
make sway
sway --validate
```

## Keys added on top of the stock config

| Key            | Action                                 |
|----------------|----------------------------------------|
| `Super+Escape` | lock the screen                        |
| `Shift+Print`  | screenshot a region to the clipboard   |
| media keys     | play/pause, next, previous (playerctl) |
