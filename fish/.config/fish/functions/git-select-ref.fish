if status is-interactive; and type -q fzf; and type -q git
  function git-select-ref --description "choose git branch or tag"
    # ctrl-x deletes the highlighted local branch and reloads the list. ctrl-a
    # reloads with remote branches included. Records come from __git_ref_records
    # (its own autoloaded function) so fzf's reload can re-run it via `fish -c`.
    # Tags (kind "tag") and remote branches (kind "remote") can't be deleted, so
    # ctrl-x is a no-op for those (column 1 check).
    set branch (__git_ref_records $argv[1] | fzf --no-hscroll --no-multi --ansi \
      --delimiter=\t \
      --list-label=' ctrl-x: delete branch | ctrl-a: include remotes ' \
      --list-label-pos='-3:bottom' \
      --color='list-label:dim' \
      --preview="git hist -n 20 --color --graph {2}" \
      --bind="ctrl-a:reload(fish -c '__git_ref_records -a')" \
      --bind="ctrl-x:execute-silent(test {1} = branch; and git branch -D {2})+reload(fish -c '__git_ref_records')")
    if test -n "$branch"
      git checkout (string split \t -f2 $branch | string replace "origin/" "")
    end
  end
end
