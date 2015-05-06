#!/usr/bin/env bash
# -*- coding: utf-8 -*-

# AUTHOR: Ruslan Kiianchuk <ruslan.kiianchuk@gmail.com>
# Install specified release of Firefox (x32 and x64 versions)

set -o errexit
set -o xtrace


DROPBOX_DEB="dropbox_1.6.2_amd64.deb"
DROPBOX_URL="https://www.dropbox.com/download?dl=packages/debian/${DROPBOX_DEB}"

wget ${DROPBOX_URL} -O ${DROPBOX_DEB}

sudo dpkg -i ${DROPBOX_DEB}
rm -f ${DROPBOX_DEB}
