# Makefile to bootstrap Vim.

VIMHOME="${HOME}/.vim"
VUNDLE="https://github.com/VundleVim/Vundle.vim.git"
DEPS="curl xkb-switch-git ttf-dejavu-sans-mono-powerline-git"
INSTALL_CMD="pacaur"
INSTALL_ARGS="-Sy"

deps:
	$(INSTALL_CMD) $(INSTALL_ARGS) $(DEPS)


init:
	mkdir -p "${VIMHOME}/backups"
	mkdir -p "${VIMHOME}/swap"

	# Setup Vundle plugin manager.
	if [ ! -d "${VIMHOME}/bundle" ]; then \
		mkdir -p "${VIMHOME}/bundle"; \
		git clone ${VUNDLE} "${VIMHOME}/bundle/Vundle.vim"; \
	fi

	# Install plugins.
	vim +PluginInstall! +qall

help:
	@echo "deps	Install dependencies required for Vim at runtime."
	@echo "init     Setup/update Vim plugins and dependencies."

.PHONY: deps init help

