function __zmx_select_transform --description "emit fzf actions for zmx-select, per key and mode"
  # Same two-mode design as __git_worktree_transform (see there for why this is
  # one fzf instance rather than a nested prompt): browse picks a session,
  # create takes a new session name. The mode flag is the ANSI-stripped prompt
  # fzf exports as FZF_PROMPT.
  set -l browse 'transform-prompt(printf "session ❯ ")+change-list-label( ctrl-a: new · ctrl-x: kill )+clear-query'
  set -l create 'transform-prompt(printf "\033[1;31mnew session ❯ \033[0m")+change-list-label( enter: create · esc: cancel )+clear-query'

  set -l mode browse
  string match -q '*new session*' -- "$FZF_PROMPT"; and set mode create

  switch "$argv[1]"
    case ctrl-a
      test $mode = browse; and echo $create; or echo ignore
    case ctrl-x
      test $mode = browse
      and echo 'execute-silent(zmx kill {})+reload(zmx ls --short 2>/dev/null)'
      or echo ignore
    case enter
      # The new name is printed from $FZF_QUERY, which fzf exports to become()
      # children, rather than interpolated into the action string where a paren
      # in the name would break parsing. printf works in fish and POSIX shells,
      # whichever $SHELL fzf runs it with.
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
