if status is-interactive; and type -q fzf; and type -q git
  function git-select-worktree --description "cd to a git worktree"
    # Each item is 2 display lines: "~path" then indented "branch commit".
    # Field 1 (real path, tab-delimited) is hidden via --with-nth and used for cd.
    set -l target (begin
      for line in (git --no-pager worktree list | string replace -ra '\s+' \t)
        set -l path (string split -f1 \t $line)
        set -l commit (string split -f2 \t $line)
        set -l branch (string split -f3 \t $line)
        printf '%s\t%s\n    %s %s\0' $path (string replace $HOME '~' $path) $branch $commit
      end
    end | fzf --read0 --gap --no-hscroll --no-multi --ansi --height=50% --layout=reverse \
      --prompt='worktree ❯ ' \
      --delimiter=\t --with-nth='2..' \
      --preview="git -C (string split \t -f1 {}) hist -n 20 --color --graph")
    if test -n "$target"
      cd (string split \t -f1 $target[1])
    end
  end
end
