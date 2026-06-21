if status is-interactive; and type -q fzf; and type -q git
  function git-select-worktree --description "cd to a git worktree"
    # Hidden subcommand: emit the NUL-delimited fzf records. Split out so the
    # picker can re-run it via fzf's reload() after a delete. fzf spawns its
    # commands under $SHELL (not necessarily fish), so reload re-invokes this
    # branch with `fish -c`.
    if test "$argv[1]" = --records
      # --porcelain gives one block per worktree, fields on their own lines,
      # so paths/branches with spaces and "(detached HEAD)" stay intact.
      set -l blob (git --no-pager worktree list --porcelain | string collect)
      for block in (string split \n\n -- $blob)
        set -l path (string match -gr '(?m)^worktree (.*)$' -- $block)
        test -z "$path"; and continue
        set -l sha (string match -gr '(?m)^HEAD (.{7})' -- $block)
        set -l ref (string match -gr '(?m)^branch refs/heads/(.*)$' -- $block)
        if test -z "$ref"  # no branch -> bare or detached
          string match -qr '(?m)^bare' -- $block; and set ref '(bare)'; or set ref '(detached)'
        end
        set -l meta $ref $sha
        string match -qr '(?m)^locked' -- $block; and set -a meta locked
        string match -qr '(?m)^prunable' -- $block; and set -a meta prunable
        # Field 1 = display (bold path + dim meta line), field 2 = real path
        # (hidden, used for cd, delete and preview). The display's embedded
        # newline must stay literal, so printf the whole record in one shot —
        # a `set` intermediate would split it on the newline.
        printf '\e[1m%s\e[0m\n    \e[2m%s\e[0m\t%s\0' (string replace -- $HOME '~' $path) (string join '  ·  ' $meta) $path
      end
      return
    end

    # ctrl-x removes the highlighted worktree and reloads the list; prune cleans
    # up entries whose dir is already gone. Enter cd's to the chosen worktree.
    set -l target (git-select-worktree --records | fzf --read0 --gap --ansi \
      --layout=reverse --height=50% \
      --prompt='worktree ❯ ' \
      --delimiter=\t --with-nth=1 --accept-nth=2 \
      --list-border=rounded \
      --list-label=' ctrl-x: remove worktree ' \
      --list-label-pos='-3:bottom' \
      --color='list-label:dim' \
      --preview="git -C {2} hist -n 20 --color --graph" \
      --bind="ctrl-x:execute-silent(git worktree remove --force {2}; git worktree prune)+reload(fish -c 'git-select-worktree --records')")
    if test -n "$target"; and test -d "$target"
      cd $target
    end
  end
end
