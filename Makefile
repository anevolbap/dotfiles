# Dotfiles managed with GNU stow

DOTFILES_DIR := $(shell pwd)
EMACS_TARGET  := $(HOME)/.emacs.d
CRKBD_TARGET  := $(HOME)/qmk_firmware/keyboards/crkbd/keymaps/anevolbap

.PHONY: emacs check delete crkbd local claude dunst sway system test help

# Default target: show available targets
help:
	@echo "Available targets:"
	@echo "  emacs   — install / re-create Emacs config symlinks via stow"
	@echo "  check   — dry-run: show what stow would do without making changes"
	@echo "  delete  — remove Emacs config symlinks"
	@echo "  crkbd   — install Corne keyboard layout symlink"
	@echo "  local   — install ~/.local/bin scripts (corne-rgb-alert)"
	@echo "  claude  — install Claude Code settings (~/.claude/settings.json)"
	@echo "  dunst   — install dunst config and notification autostart entries"
	@echo "  sway    — install sway config (~/.config/sway/config)"
	@echo "  system  — install systemd-sleep hooks (requires sudo)"
	@echo "  test    — run the script tests (pytest)"

# Install / re-create symlinks for Emacs config
emacs:
	stow --verbose --dir=$(DOTFILES_DIR) --target=$(EMACS_TARGET) --restow emacs

# Dry-run: show what stow would do without making changes
check:
	stow --verbose --simulate --dir=$(DOTFILES_DIR) --target=$(EMACS_TARGET) --restow emacs

# Remove symlinks for Emacs config
delete:
	stow --verbose --dir=$(DOTFILES_DIR) --target=$(EMACS_TARGET) --delete emacs

# Corne keyboard layout
crkbd:
	mkdir -p $(CRKBD_TARGET)
	stow --verbose --dir=$(DOTFILES_DIR) --target=$(CRKBD_TARGET) --restow crkbd_layout

# ~/.local/bin scripts (includes corne-rgb-alert)
local:
	stow --verbose --dir=$(DOTFILES_DIR) --target=$(HOME) --restow local

# Claude Code global settings (~/.claude/settings.json)
claude:
	stow --verbose --dir=$(DOTFILES_DIR) --target=$(HOME) --restow claude

# Dunst notification daemon: ~/.config/dunst and the autostart entries that
# start dunst and keep xfce4-notifyd from starting.
dunst:
	stow --verbose --dir=$(DOTFILES_DIR) --target=$(HOME) --restow dunst

# Sway window manager, run as a second session next to XFCE.
sway:
	stow --verbose --no-folding --dir=$(DOTFILES_DIR) --target=$(HOME) --restow sway

# systemd-sleep hooks (system path needs sudo; stow doesn't fit here)
system:
	sudo ln -sf $(DOTFILES_DIR)/system/systemd-sleep/restart-wifi \
		/lib/systemd/system-sleep/restart-wifi

# Tests for the scripts in local/.local/bin
test:
	python3 -m pytest tests -q
