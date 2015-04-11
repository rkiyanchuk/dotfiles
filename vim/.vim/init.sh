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

echo "Install dependencies..."
pip install --user --upgrade -r requirements.txt
