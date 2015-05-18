#!/usr/bin/env bash
# -*- coding: utf-8 -*-

# AUTHOR: Ruslan Kiianchuk <ruslan.kiianchuk@gmail.com>
# Install specified release of Firefox (x32 and x64 versions)

set -o errexit
set -o xtrace


RELEASE="38.0"
FIREFOX64_URL="https://download-installer.cdn.mozilla.net/pub/firefox/releases/${RELEASE}/linux-x86_64/en-US/firefox-${RELEASE}.tar.bz2"
FIREFOX32_URL="https://download-installer.cdn.mozilla.net/pub/firefox/releases/${RELEASE}/linux-i686/en-US/firefox-${RELEASE}.tar.bz2"

JAVA64=$(find /opt/java_x64 -name jre*)
JAVA64_VERSION=${JAVA64##*/}
JAVA32=$(find /opt/java_x64 -name jre*)
JAVA32_VERSION=${JAVA32##*/}


# Firefox x64
wget ${FIREFOX64_URL} -O firefox64.tar.bz2

bunzip2 firefox64.tar.bz2
tar -xf firefox64.tar
rm -rf firefox64.tar

rm -rf /opt/firefox_x64
mv firefox /opt/firefox_x64
mkdir /opt/firefox_x64/plugins
sudo ln -fs /opt/java_x64/${JAVA64_VERSION}/lib/amd64/libnpjp2.so /opt/firefox_x64/plugins
sudo ln -fs /opt/firefox_x64/firefox /usr/local/bin/firefox


# Firefox x32
wget ${FIREFOX32_URL} -O firefox32.tar.bz2

bunzip2 firefox32.tar.bz2
tar -xf firefox32.tar
rm -rf firefox32.tar

rm -rf /opt/firefox_x32
mv firefox /opt/firefox_x32
mkdir /opt/firefox_x32/plugins
sudo ln -fs /opt/java_x32/${JAVA32_VERSION}/lib/i386/libnpjp2.so /opt/firefox_x32/plugins

cat > /opt/firefox_x32/firefox.sh << \EOF
#!/usr/bin/env bash
# -*- coding: utf-8 -*-

export ENV_HOME=/opt
export FIREFOX_HOME=${ENV_HOME}/firefox_x32
export MOZ_PLUGIN_PATH=${FIREFOX_HOME}/plugins
export JDK_HOME=${ENV_HOME}/java_x32/\${JAVA32_VERSION}
export JAVA_HOME=${JDK_HOME}/jre
export PATH=${JAVA_HOME}/bin:${JDK_HOME}/bin:${PATH}
$FIREFOX_HOME/firefox --no-remote -P webex
EOF

chmod +x /opt/firefox_x32/firefox.sh
sudo ln -fs /opt/firefox_x32/firefox.sh /usr/local/bin/firefox32
