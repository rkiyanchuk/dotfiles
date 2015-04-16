#!/usr/bin/env bash
# coding: utf-8

# Fast forward local branches to remote state.

GREEN="\033[0;32m"
RED="\033[0;31m"
RESET="\033[0;0m"

main() {
  if [ ! -d .git ]; then
      echo "Not in git repository. Aborting."
      exit 1
  fi
  REMOTES="$*";
  if [ -z "$REMOTES" ]; then
    REMOTES=$(git remote);
  fi
  REMOTES=$(echo "$REMOTES" | xargs -n1 echo)
  CLB=$(git branch -l|awk '/^\*/{print $2}');
  echo "$REMOTES" | while read REMOTE; do
    git remote update "$REMOTE"
    git remote show "$REMOTE" -n \
    | awk '/merges with remote/{print $5" "$1}' \
    | while read line; do
      RB=$(echo "$line"|cut -f1 -d" ");
      ARB="refs/remotes/$REMOTE/$RB";
      LB=$(echo "$line"|cut -f2 -d" ");
      ALB="refs/heads/$LB";
      NBEHIND=$(( $(git rev-list --count "$ALB..$ARB" 2>/dev/null) +0));
      NAHEAD=$(( $(git rev-list --count "$ARB..$ALB" 2>/dev/null) +0));
      if [ "$NBEHIND" -gt 0 ]; then
        if [ "$NAHEAD" -gt 0 ]; then
          echo -e "${RED} > $LB is $NAHEAD commit(s) ahead of $REMOTE/$RB. Cannot be fast-forwarded.${RESET}";
        elif [ "$LB" = "$CLB" ]; then
          echo -e "${GREEN} > $LB is $NBEHIND commit(s) behind of $REMOTE/$RB.  Fast-forward merge."${RESET};
          git merge "$ARB";
        else
          echo -e "${GREEN} > $LB is $NBEHIND commit(s) behind of $REMOTE/$RB.  Reset local branch to remote.${RESET}";
          git branch -l -f "$LB" -t "$ARB";
        fi
      fi
    done
  done
}

main "$@"
