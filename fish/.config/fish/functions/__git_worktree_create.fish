function __git_worktree_create --description "add a worktree named \$FZF_QUERY under ../worktrees/<repo>"
  # Invoked from git-select-worktree's fzf create-mode binding via `fish -c`, so
  # no interactive guard. The slug arrives in $FZF_QUERY, which fzf exports to
  # execute() children -- passing it as an argument would mean interpolating
  # user text into an fzf action string, where a paren would break parsing.
  set -l slug (string trim -- (test (count $argv) -gt 0; and echo $argv[1]; or echo $FZF_QUERY))
  test -z "$slug"; and return 0

  set -l common (git rev-parse --path-format=absolute --git-common-dir 2>/dev/null)
  or return 1
  # --git-common-dir (not --show-toplevel) resolves the *main* checkout even when
  # called from inside another worktree, so worktrees never nest.
  set -l main (path dirname -- $common)
  set -l repo (path basename -- $main)
  set -l dest (path dirname -- $main)/worktrees/$repo/$slug

  if test -e $dest
    __git_worktree_create_fail "already exists: "(string replace -- $HOME '~' $dest)
    return 1
  end

  # Reuse the branch if it already exists, otherwise fork a new one off HEAD.
  if git show-ref --quiet --verify refs/heads/$slug
    git worktree add -- $dest $slug
  else
    git worktree add -b $slug -- $dest HEAD
  end
  or begin
    __git_worktree_create_fail
    return 1
  end
end

function __git_worktree_create_fail --description "pause so fzf does not wipe the error"
  test (count $argv) -gt 0; and echo $argv >&2
  # `_` is read-only in fish 4, hence a named throwaway.
  read -l -P 'press enter to continue ' discard
end
