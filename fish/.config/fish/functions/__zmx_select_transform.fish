function __zmx_select_transform --description "emit fzf actions for zmx-select, per key and mode"
  # Mode is read from the ANSI-stripped prompt fzf exports as FZF_PROMPT; see
  # __git_worktree_transform for why this is one fzf rather than two.
  set -l browse 'transform-prompt(printf "session ❯ ")+change-list-label( ctrl-a: new · ctrl-x: kill )+clear-query'
  set -l create 'transform-prompt(printf "\033[1;31mnew session ❯ \033[0m")+change-list-label( enter: create · esc: cancel )+clear-query'

  set -l mode browse
  string match -q '*new session*' -- "$FZF_PROMPT"; and set mode create

  switch "$argv[1]"
    case ctrl-a
      # fzf runs this in the caller's cwd.
      test $mode = browse
      and echo "$create+transform-query(fish -c __zmx_default_name)"
      or echo ignore
    case ctrl-x
      test $mode = browse
      and echo 'execute-silent(zmx kill {1})+reload(fish -c __zmx_sessions)'
      or echo ignore
    case enter
      # Print the name from $FZF_QUERY, not by interpolating it into the action,
      # where a paren in the name would break parsing.
      if test $mode = browse
        echo accept
      else if test -n "$(string trim -- "$FZF_QUERY")"
        echo 'become(printf "%s" "$FZF_QUERY")'
      else
        echo ignore
      end
    case esc
      # With no sessions there is nothing to browse, so esc leaves the picker.
      if test $mode = create; and test "$FZF_TOTAL_COUNT" -gt 0
        echo $browse
      else
        echo abort
      end
  end
end
