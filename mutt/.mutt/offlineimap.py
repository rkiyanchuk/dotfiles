#!/usr/bin/env python
# coding: utf-8

# Add entries to keyring with:
# secret-tool store --label='email' account gmail

import subprocess
import sys


def get_password(account):
    password = subprocess.check_output(
        ['secret-tool', 'lookup', 'account', account])
    return password

if __name__ == "__main__":
    print(get_password(sys.argv[1]))
