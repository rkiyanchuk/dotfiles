#!/usr/bin/env sh

# Download sources and compile Vim.

hg clone https://vim.googlecode.com/hg/ vim_source
pushd vim_source
hg pull
hg update
#hg update v7-4

aptitude build-dep vim

./configure --enable-pythoninterp \
    --with-python-config-dir=/usr/lib/python2.7/config \
    --enable-rubyinterp --with-features=huge --enable-gui=gtk2 \
    --with-compiledby=Zoresvit

make -j 8
popd
