# Makefile to bootstrap Vim.

VIMHOME="${HOME}/.vim"
VUNDLE="https://github.com/gmarik/vundle.git"


deps:
	# Install dev dependencies to compile Vim.
	# Requires root privileges!
	sudo aptitude update
	sudo aptitude -y build-dep vim

compile: deps
	rm -rf vim  # Clean up existing Vim source.
	git clone https://github.com/vim/vim.git
	git -C vim pull
	cd vim; configure --enable-pythoninterp \
		--with-python-config-dir=/usr/lib/python2.7/config \
		--enable-rubyinterp \
		--with-features=huge \
		--enable-gui=gtk2 \
		--with-compiledby=Zoresvit
	make -C vim -j 8

install: compile
	# Create and install Vim Deb package.
	# Requires root privileges!
	# These package parameters for checkinstall are set for Debian.
	# For other distributions fix accordingly.
	cd vim; sudo checkinstall -y \
		--pkgversion "2:$(shell git -C vim describe --tags | cut -c 2-8)" \
		--pkgsource "https://github.com/vim/vim" \
		--requires "libacl1,libc6,libgpm2,libselinux1,libtinfo5" \
		--provides editor \
		--maintainer "Ruslan Kiianchuk \<ruslan.kiianchuk@gmail.com\>"
	sudo rm -rf vim

init:
	# Setup Vundle plugin manager.
	if [ ! -d "${VIMHOME}/bundle" ]; then \
		mkdir -p "${VIMHOME}/bundle"; \
		git clone ${VUNDLE} "${VIMHOME}/bundle/vundle"; \
	fi
	vim +PluginInstall! +qall  # Install plugins.
	# Setup YouCompleteMe plugin.
	${HOME}/.vim/bundle/YouCompleteMe/install.sh --clang-completer
	# Install dependencies.
	pip install --user --upgrade -r ${HOME}/.vim/python-requirements.txt

help:
	@echo "deps       Install dev dependencies to compile Vim."
	@echo "compile    Clone and compile latest Vim source code."
	@echo "install    Create and install Vim Deb package from compiled source."
	@echo "init       Setup/update Vim plugins and dependencies."

.PHONY: deps compile install init help 
