#!/usr/bin/env bash

# Create and install Vim Deb package.
# Requires root privileges!

pushd vim
checkinstall -y
popd
rm -r vim
