function __git_ref_records --description "list git branches and tags for fzf"
  # $argv is forwarded to `git branch` (e.g. -a to include remotes). Tab-delimited
  # "kind<tab>ref" rows; the current branch (leading *) is dropped.
  git --no-pager branch $argv --format="%(color:blue bold)branch%09%(color:reset)%(refname:short)" | sed '/^\*/d'
  git --no-pager tag --format="%(color:magenta bold)tag%09%(color:reset)%(refname:short)"
end
