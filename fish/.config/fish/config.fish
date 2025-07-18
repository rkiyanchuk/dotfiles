set fish_greeting  # Clear default greeting


# ENVIRONMENT

set fish_color_valid_path  # Disable underlining path

# Customize exa colors
set -x EXA_COLORS "gu=32:uu=32:sn=35:sb=35:da=33"

# Configure fzf fuzzy finder to use GitHub Dimmed colors.
set -x FZF_DEFAULT_OPTS "--color=fg:#c9d1d9,bg:#23272d,hl:#58a6ff --color=fg+:#c9d1d9,bg+:#23272d,hl+:#58a6ff --color=info:#79c0ff,prompt:#f85149,pointer:#d2a8ff --color=marker:#3fb950,spinner:#d2a8ff,header:#3fb950"

set --export --global VISUAL nvim
set --export --global EDITOR nvim
set --export --global CLICOLOR 1  # Turn on colors for some BSD tools
set --export --global GPG_TTY (tty)  # Setup TTY for GPG pinetry

fish_add_path --global ~/.cargo/bin           # Cargo executables
fish_add_path --global ~/.go           	      # Golang executables
fish_add_path --global ~/.local/bin           # pip/pipx executables for Python
fish_add_path --global ~/node_modules/.bin    # npm executables
fish_add_path --global ~/.claude/local

if test (uname) = "Darwin"
    if type -q brew
        eval "$(brew shellenv)"
    end
    fish_add_path --global /opt/homebrew/bin/     # packages installed by homebrew
    fish_add_path --global /opt/homebrew/sbin     # packages installed by homebrew
    fish_add_path "/Applications/IntelliJ IDEA.app/Contents/MacOS" # Add IntelliJ IDEA
end

# Starship is the minimal, fast, and  customizable prompt for any shell.
if status is-interactive;
    if type -q starship
        starship init fish | source
    end
end

# ALIASES

alias l="ls -l"
alias ll="ls -al"
alias dud="du -hd1"
alias vim="nvim"
alias vimdiff="nvim -d"
alias java_home="/usr/libexec/java_home"
alias fish_reload="source ~/.config/fish/config.fish"
# Use `jq` with both JSON and non-JSON lines, dropping non-JSON.
alias jqq="jq -R 'fromjson? | select(type == \"object\")'"
# Use `jq` with both JSON and non-JSON lines, preserving non-JSON.
alias jqr="jq -R -r '. as \$line | try fromjson catch \$line'"

alias code="code --profile Minimal"


# FUNCTIONS

function urldecode
  python3 -c "import urllib.parse as url; print(url.unquote('$argv[1]'))"
end

function tree
  command tree --dirsfirst -C $argv
end

function http-serve --wraps='python3 -m http.server' --description 'Run Python HTTP server in current dir'
  python3 -m http.server $argv;
end

function ls --wraps='ls --color=auto --group-directories-first' --description 'alias ls=ls --color=auto --group-directories-first'
    if type -q eza
      command eza --group-directories-first --group $argv;
    else
      command ls --color=auto --group-directories-first $argv;
    end
end

function eza --wraps='eza --group-directories-first --group' --description 'alias eza=eza --group-directories-first --group'
  command eza --group-directories-first --group $argv;
end

function chatgpt --wraps='chatblade' --description 'alias to chatblade'
  command chatblade $argv;
end

function upgrade --description 'Upgrade all cli tools'
  switch (uname)
    case "Darwin"
      echo "=> Updating Brew..."
      brew update; brew upgrade; brew upgrade --cask; brew autoremove
    end
    if type -q fisher
        curl -sL https://raw.githubusercontent.com/jorgebucaran/fisher/main/functions/fisher.fish | source && fisher install jorgebucaran/fisher
        echo "=> Updating fish plugins via fisher..."
        fisher update
    end
end

function recent-installs --description 'Upgrade all cli tools'
  eza -l --sort time --time modified $(brew --cellar)
end

function pyclean --description "Delete all temporary Python files"
    find . \( -name \*.pyc -o -name \*.pyo -o -name __pycache__ \) -delete
end

function fzg
  set tags (git --no-pager tag --format="%1B[0;35;1mtag%09%1B[m%(refname:short)")
  set branches (git --no-pager branch $argv[1] --format="%1B[0;34;1mbranch%09%1B[m%(refname:short)" | sed '/^\*/d')
  set target (string join " " $branches $tags)
  set branch (echo $target | string split " " | fzf --no-hscroll --no-multi --ansi --preview="git log -n 20 --color --graph --decorate --abbrev-commit --date=short --pretty=format:\"%Cblue%h%Creset %C(yellow)%ad%Creset %<(16)%Cgreen%an%Creset %s %Cred%d%Creset \" {2}")
  if test -n "$branch"
    git checkout  (string split \t -f2 $branch | string replace "origin/" "")
  end
end

# Source per-host configurations as well as localhost overrides.
if status is-interactive
  for name in config.{$hostname,local}.fish
    if test -e $__fish_config_dir/$name
      source $__fish_config_dir/$name
    end
  end
end

# Disabled due to rendering artifacts during typing.
# source ~/.config/fish/.iterm2_shell_integration.fish
