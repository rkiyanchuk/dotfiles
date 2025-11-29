set fish_greeting  # Clear default greeting
set fish_color_valid_path  # Disable underlining path

# ENVIRONMENT

# Customize exa colors
set -x EXA_COLORS "gu=32:uu=32:sn=35:sb=35:da=33"

set --export --global VISUAL nvim
set --export --global EDITOR nvim
set --export --global CLICOLOR 1  # Turn on colors for some BSD tools
set --export --global GPG_TTY (tty)  # Setup TTY for GPG pinetry
set --export --global LESS "FRX"

fish_add_path --global ~/.local/bin           # User executables
fish_add_path --global ~/.cargo/bin           # Cargo executables
fish_add_path --global ~/.go           	      # Golang executables
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

if status is-interactive; and type -q zoxide
  zoxide init fish | source
end

if status is-interactive; and type -q fzf
  # Configure fzf fuzzy finder to use terminal colors.
  set -xg FZF_DEFAULT_OPTS "--style full --height ~80% --tmux 80% --prompt='❯ ' --color=bg+:#292E42,fg+:7,hl+:4 --color=info:6,prompt:6,pointer:5"

  # Disable default Ctrl-R binding of fzf to avoid conflict with fish's own Ctrl-R history search.
  fzf --fish | FZF_CTRL_R_COMMAND= source
end

# ALIASES

alias l="ls -l"
alias ll="ls -al"
alias dud="du -hd1"
alias vim="nvim"
alias java_home="/usr/libexec/java_home"
alias fish-reload="source ~/.config/fish/config.fish"
alias brew-recent="brew list -tr --installed-on-request"
# Use `jq` with both JSON and non-JSON lines, dropping non-JSON.
alias jqq="jq -R 'fromjson? | select(type == \"object\")'"
# Use `jq` with both JSON and non-JSON lines, preserving non-JSON.
alias jqr="jq -R -r '. as \$line | try fromjson catch \$line'"

if status is-interactive
  function update --description 'update all CLI tools'
    switch (uname)
      case "Darwin"
        echo "=> Updating Brew..."
        brew update; brew upgrade; brew upgrade --cask; brew autoremove
      end

      # Ensure Fisher plugin manager is installed
      if not type -q fisher
        echo "=> Fisher plugin manager not found!"
        echo "=> Installing Fisher..."
        curl -sL https://raw.githubusercontent.com/jorgebucaran/fisher/main/functions/fisher.fish | source && fisher install jorgebucaran/fisher
      end

      # Update Fish plugins via Fisher plugin manager
      echo "=> Updating fish plugins..."
      fisher update
  end
end

# BINDINGS

source $__fish_config_dir/bindings.fish

# Source per-host configurations as well as localhost overrides.
if status is-interactive
  for name in config.{$hostname,local}.fish
    if test -e $__fish_config_dir/$name
      source $__fish_config_dir/$name
    end
  end
end