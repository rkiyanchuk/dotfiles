#!/usr/bin/env python
# -*- coding: utf-8 -*-

"""
Populate specified or latest commit across all branches in the repository.

"""

import os
import re
import sys
import sh


git = sh.git.bake(_cwd=os.curdir)

if len(sys.argv) > 1:
    if '-h' in sys.argv or '--help' in sys.argv:
        print('Usage: git-populate.py [commit] [-h|--help]')
        sys.exit(0)
    else:
        commit = sys.argv[1]
else:
    # By default choose latest commit.
    git_log = git('--no-pager', 'log', '-n', '1', '--no-color').stdout
    commit = re.search(r'[a-f0-9]{40}', git_log).group()

print('Commit to be populated: {0:.6s}'.format(commit))

git_branch = git.branch('-a', '--no-color').stdout
source_branch = re.search(r'\*\s(\w+)$', git_branch, re.MULTILINE).group(1)
print('Source branch: {0}'.format(source_branch))

branches = re.findall(r'remotes/\w+/([\w-]+)$', git_branch, re.MULTILINE)
# Exclude current branch from target branches.
branches = [i for i in branches if i != source_branch]
print('Target branches: {0}'.format(branches))


print('Stashing local changes')
git_stash = git.stash().stdout

for branch in branches:
    print('Ading commit {0:.6s} to branch {1}'.format(commit, branch))
    git.checkout(branch)
    try:
        result = git('cherry-pick', commit)
    except sh.ErrorReturnCode_1:
        # Ignore diplicate cherry pick and discard changes.
        git.reset()


git.checkout(source_branch)
print('Return to branch {0}'.format(source_branch))

if not git_stash.startswith('No local changes to save'):
    print('Restoring local changes')
    git.stash.pop()
