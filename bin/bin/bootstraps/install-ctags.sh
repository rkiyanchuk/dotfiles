#!/usr/bin/env bash
# -*- coding: utf-8 -*-

# Must be run as root.

git clone https://github.com/universal-ctags/ctags.git

pushd ctags

autoreconf -vfi
./configure
make -j 8
checkinstall -y
popd
rm -rf ctags
