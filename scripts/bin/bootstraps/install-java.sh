#!/usr/bin/env bash
# -*- coding: utf-8 -*-

# AUTHOR: Ruslan Kiianchuk <ruslan.kiianchuk@gmail.com>
#

set -o errexit
set -o xtrace

JAVA32="http://javadl.sun.com/webapps/download/AutoDL?BundleId=97358"
JAVA64="http://javadl.sun.com/webapps/download/AutoDL?BundleId=97360"

# Java x32
wget $JAVA32 -O java32.tar.gz
gunzip java32.tar.gz
tar -xf java32.tar
rm -rf java32.tar
JAVA_VERSION=$(find . -name jre* | cut -c 3-)

rm -rf /opt/java_x32
mkdir -p /opt/java_x32
mv $JAVA_VERSION /opt/java_x32/

update-alternatives --install /usr/bin/java java /opt/java_x32/$JAVA_VERSION/bin/java 2000
update-alternatives --install /usr/bin/javaws javaws /opt/java_x32/$JAVA_VERSION/bin/javaws 2000

# Java x64
wget $JAVA64 -O java64.tar.gz
gunzip java64.tar.gz
tar -xf java64.tar
rm -rf java64.tar
JAVA_VERSION=$(find . -name jre* | cut -c 3-)

rm -rf /opt/java_x64
mkdir -p /opt/java_x64
mv $JAVA_VERSION /opt/java_x64/

update-alternatives --install /usr/bin/java java /opt/java_x64/$JAVA_VERSION/bin/java 1000
update-alternatives --install /usr/bin/javawsc javaws /opt/java_x64/$JAVA_VERSION/bin/javaws 1000
