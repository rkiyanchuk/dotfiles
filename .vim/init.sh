#!/usr/bin/env sh

VIMHOME="$HOME/.vim"

if [ ! -L $HOME/.vimrc ]
then
    echo "Symlinking .vimrc..."
    ln -bns $VIMHOME/.vimrc $HOME/.vimrc
fi

if [ ! -L $HOME/.gvimrc ]
then
    echo "Symlinking .gvimrc..."
    ln -bns $VIMHOME/.gvimrc $HOME/.gvimrc
fi

if [ ! -d "$VIMHOME/bundle" ]
then
    echo "Setting up Vundle..."
    mkdir -p "$VIMHOME/bundle"
    git clone https://github.com/gmarik/vundle.git $VIMHOME/bundle/vundle
    echo "Installing plugins..."
    vim +BundleInstall! +qall
    echo "Done."
fi

