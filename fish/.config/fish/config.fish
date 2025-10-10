set fish_greeting  # Clear default greeting
set fish_color_valid_path  # Disable underlining path

# ENVIRONMENT

# Customize exa colors
set -x EXA_COLORS "gu=32:uu=32:sn=35:sb=35:da=33"

# Configure fzf fuzzy finder to use GitHub Dimmed colors.
set -x FZF_DEFAULT_OPTS "--color=fg:#C9D1D9,bg:#1C2128,hl:#58A6FF --color=fg+:#C9D1D9,bg+:#1C2128,hl+:#58A6FF --color=info:#79C0FF,prompt:#F85149,pointer:#D2A8FF --color=marker:#3FB950,spinner:#D2A8FF,header:#3FB950"

set --export --global VISUAL nvim
set --export --global EDITOR nvim
set --export --global CLICOLOR 1  # Turn on colors for some BSD tools
set --export --global GPG_TTY (tty)  # Setup TTY for GPG pinetry
set --export --global LESS "FRX"

fish_add_path --global ~/.local/bin           # User executables
fish_add_path --global ~/.cargo/bin           # Cargo executables
fish_add_path --global ~/.go           	      # Golang executables
fish_add_path --global ~/node_modules/.bin    # NPM executables
fish_add_path --global ~/.claude/local        # Claude code

if test (uname) = "Darwin"
    if type -q brew
        eval "$(brew shellenv)"
    end

    alias python="python3"
    alias claude="~/.claude/local/claude"

    fish_add_path --global /opt/homebrew/bin/     # packages installed by homebrew
    fish_add_path --global /opt/homebrew/sbin     # packages installed by homebrew
    fish_add_path "/Applications/IntelliJ IDEA.app/Contents/MacOS" # Add IntelliJ IDEA

    # Added by OrbStack: command-line tools and integration
    source ~/.orbstack/shell/init2.fish 2>/dev/null || :
end

# Starship is the minimal, fast, and  customizable prompt for any shell.
if status is-interactive; and type -q starship
    starship init fish | source
end

# ALIASES

alias l="ls -l"
alias ll="ls -al"
alias dud="du -hd1"
alias vim="nvim"
alias java_home="/usr/libexec/java_home"
alias fish_reload="source ~/.config/fish/config.fish"
# Use `jq` with both JSON and non-JSON lines, dropping non-JSON.
alias jqq="jq -R 'fromjson? | select(type == \"object\")'"
# Use `jq` with both JSON and non-JSON lines, preserving non-JSON.
alias jqr="jq -R -r '. as \$line | try fromjson catch \$line'"


# FUNCTIONS
if status is-interactive; and type -q python
  function urldecode --description "Decode URL string"
    python -c "import urllib.parse as url; print(url.unquote('$argv[1]'))"
  end
end

if status is-interactive; and type -q tree
  function tree --wraps='tree --dirsfirst -C' --description 'alias tree=tree --dirsfirst -C'
    command tree --dirsfirst -C $argv
  end
end

if status is-interactive; and type -q python
  function http-serve --wraps='python -m http.server' --description 'Run Python HTTP server in current dir'
    python -m http.server $argv;
  end
end


if status is-interactive
  function ls --wraps='ls --color=auto --group-directories-first' --description 'alias ls=ls --color=auto --group-directories-first'
      if type -q eza
        command eza --group-directories-first --group $argv;
      else
        command ls --color=auto --group-directories-first $argv;
      end
  end
end

if status is-interactive; and type -q eza
  function eza --wraps='eza --group-directories-first --group' --description 'alias eza=eza --group-directories-first --group'
    command eza --group-directories-first --group $argv;
  end
end

if status is-interactive
  function update --description 'Update all CLI tools'
    switch (uname)
      case "Darwin"
        echo "=> Updating Brew..."
        brew update; brew upgrade; brew upgrade --cask; brew autoremove
      end
      # Update Fish plugins via Fisher plugin manager
      if type -q fisher
          curl -sL https://raw.githubusercontent.com/jorgebucaran/fisher/main/functions/fisher.fish | source && fisher install jorgebucaran/fisher
          echo "=> Updating fish plugins via fisher..."
          fisher update
      end
  end
end

if status is-interactive; and type -q brew
  function brew-recent-installs --description 'List all manual installed CLI tools'
    ls -l --sort time --time modified $(brew --cellar)
  end
end

if status is-interactive; and type -q find
  function pyclean --description "Delete all temporary Python files"
      find . \( -name \*.pyc -o -name \*.pyo -o -name __pycache__ \) -delete
  end
end

if status is-interactive; and type -q fzf; and type -q git
  function fzg
    set tags (git --no-pager tag --format="%1B[0;35;1mtag%09%1B[m%(refname:short)")
    set branches (git --no-pager branch $argv[1] --format="%1B[0;34;1mbranch%09%1B[m%(refname:short)" | sed '/^\*/d')
    set target (string join " " $branches $tags)
    set branch (echo $target | string split " " | fzf --no-hscroll --no-multi --ansi --preview="git hist -n 20 --color --graph {2}")
    if test -n "$branch"
      git checkout  (string split \t -f2 $branch | string replace "origin/" "")
    end
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
