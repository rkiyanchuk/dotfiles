#!/usr/bin/env sh

# Create Vim Deb package.

pushd vim_source
checkinstall -y
popd
rm -r vim_source
