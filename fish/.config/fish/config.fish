set fish_greeting  # Clear default greeting


# ENVIRONMENT

set --export --global VISUAL nvim
set --export --global EDITOR nvim
set --export --global CLICOLOR 1  # Turn on colors for some BSD tools
set --export --global GPG_TTY (tty)  # Setup TTY for GPG pinetry

fish_add_path --global ~/.cargo/bin           # Cargo executables
fish_add_path --global ~/.go           	      # Golang executables
fish_add_path --global ~/.local/bin           # pip/pipx executables for Python
fish_add_path --global ~/node_modules/.bin    # npm executables

if test (uname) = "Darwin"
    fish_add_path --global /opt/homebrew/bin/     # Packages installed by Homebrew
    fish_add_path /Applications/IntelliJ IDEA.app/Contents/MacOS # Add IntelliJ IDEA
end


# ALIASES

alias l="ll"
alias dud="du -hd1"
alias pyclean="find . \( -name \*.pyc -o -name \*.pyo -o -name __pycache__ \) -delete"
alias vim="nvim"
alias vimdiff="nvim -d"
# Use `jq` with both JSON and non-JSON lines, dropping non-JSON.
alias jqq="jq -R 'fromjson? | select(type == \"object\")'"
# Use `jq` with both JSON and non-JSON lines, preserving non-JSON.
alias jqr="jq -R -r '. as \$line | try fromjson catch \$line'"


# FUNCTIONS

function urldecode
  python3 -c "import urllib.parse as url; print(url.unquote('$argv[1]'))"
end

function tree
  command tree --dirsfirst -C $argv
end

function http-serve --wraps='python3 -m http.server' --description 'alias runhttp=python3 -m http.server'
  python3 -m http.server $argv;
end

function ls --wraps='ls --color=auto --group-directories-first' --description 'alias ls=ls --color=auto --group-directories-first'
  switch (uname)
    case "Linux"
      command ls --color=auto --group-directories-first $argv;
    case "Darwin"
      if type -q exa
        command exa --group-directories-first --group $argv;
      else
        command ls $argv;
      end
  end
end

function exa --wraps='exa --group-directories-first --group' --description 'alias exa=exa --group-directories-first --group'
  command exa --group-directories-first --group $argv;
end


# PROMPT

# Starship is the minimal, fast, and  customizable prompt for any shell.
if status is-interactive; and type -q starship
  starship init fish | source
end

# Source per-host configurations as well as localhost overrides.
if status is-interactive
  for name in config.{$hostname,local}.fish
    if test -e $__fish_config_dir/$name
      source $__fish_config_dir/$name
    end
  end
end
