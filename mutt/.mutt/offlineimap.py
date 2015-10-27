#!/usr/bin/env python
# coding: utf-8

import re
import os
import sys


def get_password(account):
    """Obtain password for specified account from `secret.yaml` """
    with open(os.environ['HOME'] + '/.mutt/secret.yaml') as creds:
        text = creds.read()
        match = re.search(r"{acc}: '(.+)'".format(acc=account), text)
        password = match.group(1)
        return password

if __name__ == "__main__":
    print(get_password(sys.argv[1]))
