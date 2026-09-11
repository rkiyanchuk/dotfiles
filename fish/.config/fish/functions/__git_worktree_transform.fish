function __git_worktree_transform --description "emit fzf actions for git-select-worktree, per key and mode"
  # git-select-worktree has two modes in one fzf instance: browse (pick a
  # worktree) and create (type a slug). A nested fzf or fish `read` was the
  # obvious alternative, but a child inherits the parent fzf's termios, so
  # fzf's lone-escape detection -- which relies on a read timeout -- never
  # fires and escape is swallowed. Staying in one process keeps escape native.
  #
  # The mode flag is the prompt itself: fzf exports FZF_PROMPT to transform
  # children with ANSI stripped, so the red create prompt reads back as plain
  # text and needs no extra state file.
  set -l browse 'transform-prompt(printf "worktree ❯ ")+change-list-label( ctrl-a: add · ctrl-x: remove )+clear-query'
  set -l create 'transform-prompt(printf "\033[1;31mnew worktree ❯ \033[0m")+change-list-label( enter: create · esc: cancel )+clear-query'
  set -l records 'reload(fish -c __git_worktree_records)'

  set -l mode browse
  string match -q '*new worktree*' -- "$FZF_PROMPT"; and set mode create

  switch "$argv[1]"
    case ctrl-a
      test $mode = browse; and echo $create; or echo ignore
    case enter
      # In create mode the slug is $FZF_QUERY, read by __git_worktree_create
      # itself. execute (not execute-silent) so git's output and any error
      # pause are visible.
      test $mode = create
      and echo "execute(fish -c __git_worktree_create)+$records+$browse"
      or echo accept
    case ctrl-x
      test $mode = browse
      and echo "execute-silent(git worktree remove --force {2}; git worktree prune)+$records"
      or echo ignore
    case esc
      test $mode = create; and echo $browse; or echo abort
  end
end
