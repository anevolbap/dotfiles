# Dotfiles managed with GNU stow

DOTFILES_DIR := $(shell pwd)
EMACS_TARGET  := $(HOME)/.emacs.d
CRKBD_TARGET  := $(HOME)/qmk_firmware/keyboards/crkbd/keymaps/anevolbap

.PHONY: emacs check delete crkbd local claude help

# Default target: show available targets
help:
	@echo "Available targets:"
	@echo "  emacs   — install / re-create Emacs config symlinks via stow"
	@echo "  check   — dry-run: show what stow would do without making changes"
	@echo "  delete  — remove Emacs config symlinks"
	@echo "  crkbd   — install Corne keyboard layout symlink"
	@echo "  local   — install ~/.local/bin scripts (corne-rgb-alert)"
	@echo "  claude  — install Claude Code settings (~/.claude/settings.json)"

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
