#!/usr/bin/env bash
# -*- coding: utf-8 -*-

# AUTHOR: Ruslan Kiianchuk <ruslan.kiianchuk@gmail.com>
# 
 
FIREFOX_URL="https://download.mozilla.org/?product=firefox-29.0.1-SSL&os=linux64&lang=en-US"

wget ${FIREFOX_URL} -O firefox.tar.bz2

bunzip2 firefox.tar.bz2
tar -xf firefox.tar
rm -rf firefox.tar

rm -rf /opt/firefox
mv firefox /opt/
sudo ln -fs /opt/firefox/firefox /usr/local/bin/firefox
