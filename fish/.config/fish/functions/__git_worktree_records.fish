function __git_worktree_records --description "emit NUL-delimited git worktree records for fzf"
  # --porcelain gives one block per worktree, fields on their own lines, so
  # paths/branches with spaces and "(detached HEAD)" stay intact. No interactive
  # guard: fzf's reload() re-runs this via `fish -c`, which is non-interactive.
  set -l blob (git --no-pager worktree list --porcelain | string collect)
  for block in (string split \n\n -- $blob)
    set -l path (string match -gr '(?m)^worktree (.*)$' -- $block)
    test -z "$path"; and continue
    set -l sha (string match -gr '(?m)^HEAD (.{7})' -- $block)
    set -l ref (string match -gr '(?m)^branch refs/heads/(.*)$' -- $block)
    if test -z "$ref"  # no branch -> bare or detached
      string match -qr '(?m)^bare' -- $block; and set ref '(bare)'; or set ref '(detached)'
    end
    set -l meta $sha $ref
    string match -qr '(?m)^locked' -- $block; and set -a meta locked
    string match -qr '(?m)^prunable' -- $block; and set -a meta prunable
    # Field 1 = display (bold path + dim meta line), field 2 = real path (hidden,
    # used for cd, delete and preview). The display's embedded newline must stay
    # literal, so printf the whole record in one shot — a `set` intermediate
    # would split it on the newline.
    printf '\e[1m%s\e[0m\n    \e[2m%s\e[0m\t%s\0' (string replace -- $HOME '~' $path) (string join '  ·  ' $meta) $path
  end
end
