# Makefile to bootstrap Vim.

VIMHOME="${HOME}/.vim"
VUNDLE="https://github.com/gmarik/vundle.git"
XKB_SWITCH="https://github.com/ierton/xkb-switch.git"
LANGTOOL_VER=3.1
LANGUAGETOOL="https://www.languagetool.org/download/LanguageTool-${LANGTOOL_VER}.zip"


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

languagetool:
	# Install LanguageTool (https://www.languagetool.org) for grammar and spell
	# check. Required by vim-languagetool plugin.
	wget ${LANGUAGETOOL} -O languagetool.zip
	unzip -o languagetool.zip
	rm -rf languagetool.zip
	sudo rm -rf /opt/languagetool
	sudo mv LanguageTool-${LANGTOOL_VER} /opt/languagetool

xkb-switch:
	# Compile and install xkb-switch program for automated switching and
	# querying of current keyboard layout. Required by vim-xkbswitch plugin.
	git clone ${XKB_SWITCH}
	cd xkb-switch; cmake .
	make -C xkb-switch -j 8
	sudo mv xkb-switch/xkb-switch /usr/local/bin/
	sudo mv xkb-switch/libxkbswitch.so /usr/local/lib/
	rm -rf xkb-switch

init: languagetool xkb-switch
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
	@echo "deps            Install dev dependencies to compile Vim."
	@echo "compile         Clone and compile latest Vim source code."
	@echo "install         Create and install Vim Deb package from compiled source."
	@echo "languagetool    Install LanguageTool grammar and spell checker."
	@echo "xkb-switch      Compile and install xkb-switch for switching and quering keyboard layouts."
	@echo "init            Setup/update Vim plugins and dependencies."

.PHONY: deps compile install languagetool xkb-switch init help
