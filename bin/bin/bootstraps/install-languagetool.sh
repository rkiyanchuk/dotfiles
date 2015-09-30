#!/usr/bin/env bash
# -*- coding: utf-8 -*-

# AUTHOR: Ruslan Kiianchuk <ruslan.kiianchuk@gmail.com>
# Install specified release of Language Tool (https://www.languagetool.org/)

set -o errexit
set -o xtrace


RELEASE="3.1"
URL="https://www.languagetool.org/download/LanguageTool-${RELEASE}.zip"

wget ${URL} -O languagetool.zip

unzip languagetool.zip
rm -rf languagetool.zip

rm -rf /opt/languagetool
mv LanguageTool-${RELEASE} /opt/languagetool
