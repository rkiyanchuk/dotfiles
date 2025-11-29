set fish_greeting  # Clear default greeting
set fish_color_valid_path  # Disable underlining path

# ENVIRONMENT

# Customize exa colors
set -x EXA_COLORS "gu=32:uu=32:sn=35:sb=35:da=33"

# Configure fzf fuzzy finder to use terminal colors.
set -x FZF_DEFAULT_OPTS "--color=fg:#C9D1D9,hl:#58A6FF --color=fg+:#C9D1D9,hl+:#58A6FF --color=info:#79C0FF,prompt:#F85149,pointer:#D2A8FF --color=marker:#3FB950,spinner:#D2A8FF,header:#3FB950"

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

if status is-interactive; and type -q fzf
  # Configure fzf fuzzy finder to use terminal colors.
  set -xg FZF_DEFAULT_OPTS "--style full --height ~80% --tmux 80% --prompt='❯ ' --color=bg+:#292E42,fg+:#C9D1D9,hl+:#58A6FF --color=info:#79C0FF,prompt:#F85149,pointer:#F7768E --color=marker:#3FB950,spinner:#D2A8FF,header:#3FB950"

  # Disable default Ctrl-R binding of fzf to avoid conflict with fish's own Ctrl-R history search.
  fzf --fish | FZF_CTRL_R_COMMAND= source
end

if status is-interactive; and type -q zoxide
  zoxide init fish | source
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
  function urldecode --description "decode URL string"
    python -c "import urllib.parse as url; print(url.unquote('$argv[1]'))"
  end
end

if status is-interactive; and type -q tree
  function tree --wraps='tree --dirsfirst -C' --description 'alias tree=tree --dirsfirst -C'
    command tree --dirsfirst -C $argv
  end
end

if status is-interactive; and type -q python
  function http-serve --wraps='python -m http.server' --description 'run Python HTTP server in current dir'
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
  function update --description 'update all CLI tools'
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
  function brew-recent-installs --description 'list all manually installed Brew packages'
    ls -l --sort time --time modified $(brew --cellar)
  end
end

if status is-interactive; and type -q find
  function pyclean --description "delete all temporary Python files"
      find . \( -name \*.pyc -o -name \*.pyo -o -name __pycache__ \) -delete
  end
end

if status is-interactive; and type -q fzf; and type -q git
  function __git_branch --description "choose git branch"
    set branches (git --no-pager branch $argv[1] --format="%(color:blue bold)branch%09%(color:reset)%(refname:short)" | sed '/^\*/d')
    set target (string join " " $branches)
    set branch (echo $target | string split " " | fzf --no-hscroll --no-multi --ansi --preview="git hist -n 20 --color --graph {2}")
    if test -n "$branch"
      git checkout  (string split \t -f2 $branch | string replace "origin/" "")
    end
  end
end

if status is-interactive; and type -q fzf; and type -q git
  function __git_branch_or_tag --description "choose git branch or tag"
    set tags (git --no-pager tag --format="%(color:magenta bold)tag%09%(color:reset)%(refname:short)")
    set branches (git --no-pager branch $argv[1] --format="%(color:blue bold)branch%09%(color:reset)%(refname:short)" | sed '/^\*/d')
    set target (string join " " $branches $tags)
    set branch (echo $target | string split " " | fzf --no-hscroll --no-multi --ansi --preview="git hist -n 20 --color --graph {2}")
    if test -n "$branch"
      git checkout  (string split \t -f2 $branch | string replace "origin/" "")
    end
  end

  bind \cb __git_branch_or_tag
end

if status is-interactive; and type -q lazygit
    function gg --description "lazygit"
        lazygit
    end
end

if status is-interactive; and type -q yazi
    function y --description "yazi file manager"
        set tmp (mktemp -t "yazi-cwd.XXXXXX")
        yazi $argv --cwd-file="$tmp"
        if read -z cwd < "$tmp"; and [ -n "$cwd" ]; and [ "$cwd" != "$PWD" ]
            builtin cd -- "$cwd"
        end
        rm -f -- "$tmp"
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
