# Functions for Git

if status is-interactive; and type -q fzf; and type -q git
  function git-select-ref --description "choose git branch or tag"
    set tags (git --no-pager tag --format="%(color:magenta bold)tag%09%(color:reset)%(refname:short)")
    set branches (git --no-pager branch $argv[1] --format="%(color:blue bold)branch%09%(color:reset)%(refname:short)" | sed '/^\*/d')
    set target (string join " " $branches $tags)
    set branch (echo $target | string split " " | fzf --no-hscroll --no-multi --ansi --preview="git hist -n 20 --color --graph {2}")
    if test -n "$branch"
      git checkout  (string split \t -f2 $branch | string replace "origin/" "")
    end
  end
end

function __git_select_ref_widget --description "wrapper for git_select_ref key binding"
    git-select-ref
    commandline -f repaint
end
