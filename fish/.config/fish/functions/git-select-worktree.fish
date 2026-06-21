if status is-interactive; and type -q fzf; and type -q git
  function git-select-worktree --description "cd to a git worktree"
    # ctrl-x removes the highlighted worktree and reloads the list; prune cleans
    # up entries whose dir is already gone. Enter cd's to the chosen worktree.
    # Records come from __git_worktree_records (its own autoloaded function) so
    # fzf's reload can re-run it via `fish -c` after a delete.
    set -l target (__git_worktree_records | fzf --read0 --gap --ansi \
      --layout=reverse --height=50% \
      --prompt='worktree ❯ ' \
      --delimiter=\t --with-nth=1 --accept-nth=2 \
      --list-border=rounded \
      --list-label=' ctrl-x: remove worktree ' \
      --list-label-pos='-3:bottom' \
      --color='list-label:dim' \
      --preview="git -C {2} hist -n 20 --color --graph" \
      --bind="ctrl-x:execute-silent(git worktree remove --force {2}; git worktree prune)+reload(fish -c __git_worktree_records)")
    if test -n "$target"; and test -d "$target"
      cd $target
    end
  end
end
