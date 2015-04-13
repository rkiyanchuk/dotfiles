#!/usr/bin/env sh

# Populate configuration files
VIMHOME="$HOME/.vim"

# Setup bundle plugin manager
if [ ! -d "$VIMHOME/bundle" ]
then
    echo "Setting up Vundle..."
    mkdir -p "$VIMHOME/bundle"
    git clone https://github.com/gmarik/vundle.git "$VIMHOME/bundle/vundle"

    echo "Install plugins..."
    vim +BundleInstall! +qall
    echo "Done."
fi

pushd "${HOME}/.vim/bundle/YouCompleteMe"
./install.sh --clang-completer
popd

pushd "${HOME}/.vim/bundle/command-t/ruby/command-t"
ruby extconf.rb
make
popd

echo "Install dependencies..."
pip install --user --upgrade -r requirements.txt
