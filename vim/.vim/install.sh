#!/usr/bin/env bash

# Create Vim Deb package.

pushd vim_src
checkinstall -y
popd
rm -r vim_src
