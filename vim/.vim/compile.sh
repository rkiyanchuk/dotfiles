#!/usr/bin/env bash

# Download source code and compile Vim.

git clone https://github.com/vim/vim.git
pushd vim
git pull

aptitude build-dep vim

./configure --enable-pythoninterp \
    --with-python-config-dir=/usr/lib/python2.7/config \
    --enable-rubyinterp --with-features=huge --enable-gui=gtk2 \
    --with-compiledby=Zoresvit

make -j 8
popd
