#!/bin/bash

# Requirements:
# aptitude install cmake qt4-qmake cmake-qt-gui libqt4-dev libgcrypt11-dev


git clone https://github.com/keepassx/keepassx.git
cd keepassx
mkdir build
cd build
cmake ..
make -j8
sudo checkinstall
cd ..
cd ..
rm -rf keepassx

# Run checkinstall to install as package.
