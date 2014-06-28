#!/usr/bin/env bash
# -*- coding: utf-8 -*-

# AUTHOR: Ruslan Kiianchuk <ruslan.kiianchuk@gmail.com>
# 
 
FIREFOX_URL="https://download-installer.cdn.mozilla.net/pub/firefox/releases/30.0/linux-x86_64/en-US/firefox-30.0.tar.bz2"

wget ${FIREFOX_URL} -O firefox.tar.bz2

bunzip2 firefox.tar.bz2
tar -xf firefox.tar
rm -rf firefox.tar

rm -rf /opt/firefox
mv firefox /opt/
sudo ln -fs /opt/firefox/firefox /usr/local/bin/firefox
