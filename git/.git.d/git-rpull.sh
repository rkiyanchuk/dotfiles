#!/usr/bin/env bash
# coding: utf-8

# Do `git pull` on all git repositories in current directory recursively.

BOLD="\033[0;1m"
RESET="\033[0;0m"

find -type d -name .git -exec bash -c "cd \"{}\"/../ && echo -ne \"${BOLD}\";
    pwd; echo -ne \"${RESET}\" && git rffwd" \;
