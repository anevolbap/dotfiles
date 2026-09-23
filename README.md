# dotfiles

Configuration for Emacs, a Corne keyboard, and a Debian desktop (XFCE, with
sway as a second session). Personal values (org directory, feeds, keyboard
layout, and so on) are not in this repo. They are read from override files
if present, so a fresh clone works with neutral defaults. See
[Personal overrides](#personal-overrides).

## Layout

Most directories are [GNU stow](https://www.gnu.org/software/stow/)
packages. The `Makefile` links each one into place.

| Directory      | What it holds                                   | Linked into                  |
|----------------|-------------------------------------------------|------------------------------|
| `emacs/`       | Emacs 31 config (elpaca, eglot, org, eshell)    | `~/.emacs.d`                 |
| `crkbd_layout/`| QMK keymap for the Corne (crkbd) keyboard       | `~/qmk_firmware/.../keymaps` |
| `local/`       | Scripts in `~/.local/bin` and a systemd drop-in | `~`                          |
| `claude/`      | Claude Code agents and commands                 | `~`                          |
| `dunst/`       | dunst notification daemon, replaces xfce4-notifyd | `~`                        |
| `sway/`        | sway window manager config                      | `~`                          |
| `system/`      | systemd-sleep hook (needs root, uses `ln`)      | `/lib/systemd/system-sleep`  |

Other directories:

- `docs/`: longer notes, like how the Corne RGB alert works.
- `scripts/`: `build-emacs.sh`, builds Emacs from source.
- `tests/`: pytest tests for the scripts in `local/.local/bin`.

## Install

Clone the repo, then install one package at a time:

```bash
make            # list targets
make emacs      # link emacs/ into ~/.emacs.d
make check      # dry run for the emacs package
make test       # run the script tests
```

Each target runs `stow --restow`, so running it again is safe.

## Emacs

`emacs/init.el` bootstraps elpaca and loads the modules in order.
`settings.org` is the literate source and `settings.el` is its tangled
output; both are committed. Completion uses built-in icomplete-vertical with
orderless, consult, embark, corfu, and cape. Python uses eglot with the
[ty](https://github.com/astral-sh/ty) server through `uvx`.

## Scripts

- `claude-branch-check`: Claude Code hook that blocks commits on shared branches.
- `claude-token-report`: shows where Claude Code tokens go, from the transcripts.
- `corne-rgb-alert`: makes the Corne blink over Raw HID (see [docs/corne-rgb-alert.md](docs/corne-rgb-alert.md)).
- `notify-when-done`: runs a command and sends a desktop notification when it ends.
- `roam-problem`: writes a problem and fix note into org-roam (`$ROAM_DIR`, default `~/org/roam-notes`).
- `sleep-doctor`: finds out why a resume from suspend ended in a black screen.

## Personal overrides

Each of these is optional. Keep them in a private repo or wherever you like.

| File | Read by | What goes there |
|------|---------|-----------------|
| `~/.emacs.d/local.el` | `init.el`, before the modules | `my-org-directory`, `my-org-capture-templates-file`, `my-org-read-tag`, `my-org-todo-keyword-faces`, `my-world-clock-list`, extra packages |
| `~/.emacs.d/rss-feeds` | elfeed | a list of feeds, same format as `elfeed-feeds` |
| `~/.emacs.d/radios` | eradio | a list of `(name . url)` stations |
| `~/.config/sway/config.d/*` | sway | keyboard layout, outputs |

Environment variables: `ROAM_DIR` for `roam-problem`, `EMACS_SRC` for
`scripts/build-emacs.sh`.

## License

MIT, see [LICENSE](LICENSE). The files in `crkbd_layout/` come from QMK
firmware templates and stay under GPL-2.0-or-later, as their headers say.
