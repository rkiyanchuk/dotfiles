#!/usr/bin/env python
# coding: utf-8

import re
import os


def get_password():
    with open(os.environ['HOME'] + '/.mutt/account.gmail') as f:
        text = f.read()
        password = re.search(r"imap_pass = '(.+)'", text).group(1)
        return password
