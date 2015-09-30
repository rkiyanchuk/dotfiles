#!/bin/sh

git clone https://github.com/ierton/xkb-switch.git

pushd xkb-switch
    cmake .
    make
    mv xkb-switch /usr/local/bin/
    mv libxkbswitch.so /usr/local/lib/
popd
rm -rf xkb-switch
