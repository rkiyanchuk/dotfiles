#!/usr/bin/env bash
# -*- coding: utf-8 -*-

# AUTHOR: Ruslan Kiianchuk <ruslan.kiianchuk@gmail.com>
# <+description+>
 

DOTFILES="bash git python synergy themes vim xmonad"

STOWARGS="-vv -t ${HOME}"

stow ${STOWARGS} ${DOTFILES}
