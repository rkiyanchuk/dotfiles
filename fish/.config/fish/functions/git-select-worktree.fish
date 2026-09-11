if status is-interactive; and type -q fzf; and type -q git
  function git-select-worktree --description "cd to a git worktree"
    # Two modes in one fzf: browse (enter cd's, ctrl-x removes) and create
    # (ctrl-a switches in, type a slug, enter creates, esc backs out). Every
    # mode-sensitive key routes through __git_worktree_transform, which prints
    # the actions to run; see that function for why this isn't a nested prompt.
    # Records come from __git_worktree_records so reload can re-run it via
    # `fish -c` after a create or delete.
    set -l target (__git_worktree_records | fzf --read0 --gap --ansi \
      --layout=reverse --height=50% \
      --prompt='worktree ❯ ' \
      --delimiter=\t --with-nth=1 --accept-nth=2 \
      --list-border=rounded \
      --list-label=' ctrl-a: add · ctrl-x: remove ' \
      --list-label-pos='-3:bottom' \
      --color='list-label:dim' \
      --preview="git -C {2} hist -n 20 --color --graph" \
      --bind="ctrl-a:transform(fish -c '__git_worktree_transform ctrl-a')" \
      --bind="enter:transform(fish -c '__git_worktree_transform enter')" \
      --bind="ctrl-x:transform(fish -c '__git_worktree_transform ctrl-x')" \
      --bind="esc:transform(fish -c '__git_worktree_transform esc')")
    if test -n "$target"; and test -d "$target"
      cd $target
    end
  end
end
