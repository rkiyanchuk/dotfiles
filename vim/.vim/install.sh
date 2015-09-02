#!/usr/bin/env bash

# Create and install Vim Deb package.
# Requires root privileges!

pushd vim_src
checkinstall -y
popd
rm -r vim_src
