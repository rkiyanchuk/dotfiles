#!/usr/bin/env bash
# -*- coding: utf-8 -*-

# AUTHOR: Ruslan Kiianchuk <ruslan.kiianchuk@gmail.com>
# Set up perforce clients.

wget -N "http://filehost.perforce.com/perforce/r14.1/bin.linux26x86_64/p4v.tgz"
wget -N "http://filehost.perforce.com/perforce/r14.1/bin.linux26x86_64/p4"
gunzip -f p4v.tgz
tar -xf p4v.tar
rm p4v.tar

mkdir -p /opt/perforce
cp -r p4v-* /opt/perforce/p4v
cp p4 /opt/perforce
rm -rf p4v-*
rm -rf p4

chmod -R 755 /opt/perforce/*
ln -s /opt/perforce/p4 /usr/local/bin/p4
ln -s /opt/perforce/p4v/bin/p4v /usr/local/bin/p4v
