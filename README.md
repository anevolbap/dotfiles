# dotfiles

My personal configuration for Emacs, a Corne keyboard, and a Debian desktop
(XFCE, with sway as a second session). It is shared as a reference. It is
not a general-purpose setup, so expect paths and hardware assumptions that
match my machine (a laptop running Debian).

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

- `backup-upgrade`: snapshot config, documents and code before a Debian release upgrade.
- `claude-branch-check`: Claude Code hook that blocks commits on shared branches.
- `claude-token-report`: shows where Claude Code tokens go, from the transcripts.
- `corne-rgb-alert`: makes the Corne blink over Raw HID (see [docs/corne-rgb-alert.md](docs/corne-rgb-alert.md)).
- `notify-when-done`: runs a command and sends a desktop notification when it ends.
- `roam-problem`: writes a problem and fix note into org-roam.
- `sleep-doctor`: finds out why a resume from suspend ended in a black screen.

## License

MIT, see [LICENSE](LICENSE). The files in `crkbd_layout/` come from QMK
firmware templates and stay under GPL-2.0-or-later, as their headers say.
