#!/usr/bin/env bash

# Download source code and compile Vim.

hg clone https://vim.googlecode.com/hg/ vim_src
pushd vim_src
hg pull
hg update

# Uncomment to get specific version instead of master.
#hg update v7-4

aptitude build-dep vim

./configure --enable-pythoninterp \
    --with-python-config-dir=/usr/lib/python2.7/config \
    --enable-rubyinterp --with-features=huge --enable-gui=gtk2 \
    --with-compiledby=Zoresvit

make -j 8
popd
