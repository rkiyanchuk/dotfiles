function __git_ref_records --description "list git branches and tags for fzf"
  # Tab-delimited "kind<tab>ref" rows: local branches (purple), tags (cyan) and,
  # when -a is passed, remote branches (yellow). The remote HEAD symref
  # (origin/HEAD -> origin/main) is dropped since it is not a checkoutable ref.
  # --color=always is required: output is piped into fzf, so git would otherwise
  # drop the %(color:...) placeholders.
  git --no-pager branch --color=always \
    --format="%(color:magenta bold)branch%09%(color:reset)%(refname:short)"
  git --no-pager tag --color=always \
    --format="%(color:cyan bold)tag%09%(color:reset)%(refname:short)"
  if contains -- -a $argv
    git --no-pager branch -r --color=always \
      --format="%(if)%(symref)%(then)%(else)%(color:yellow bold)remote%09%(color:reset)%(refname:short)%(end)" \
      | sed '/^$/d'
  end
end
