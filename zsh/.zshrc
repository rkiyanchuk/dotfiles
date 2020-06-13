# Enable Powerlevel10k instant prompt. Should stay close to the top of ~/.zshrc.
# Initialization code that may require console input (password prompts, [y/n]
# confirmations, etc.) must go above this block; everything else may go below.
if [[ -r "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh" ]]; then
  source "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh"
fi

# Path to your oh-my-zsh installation.
export ZSH="${HOME}/.oh-my-zsh"

# Set name of the theme to load
# See https://github.com/ohmyzsh/ohmyzsh/wiki/Themes
#ZSH_THEME="robbyrussell"
ZSH_THEME="powerlevel10k/powerlevel10k"

# Uncomment the following line to display red dots whilst waiting for completion.
COMPLETION_WAITING_DOTS="true"

HIST_STAMPS="yyyy-mm-dd"

# Which plugins would you like to load?
# Standard plugins can be found in ~/.oh-my-zsh/plugins/*
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
# Add wisely, as too many plugins slow down shell startup.
plugins=(
    colored-man-pages
)

source $ZSH/oh-my-zsh.sh


# USER SETTINGS

export EDITOR="nvim"
export GPG_TTY=$(tty)
export GOPATH="$HOME/.go"

export PATH="/usr/local/sbin:$PATH"
export PATH="$HOME/.local/bin:$PATH"  # Scripts installed by pip (Python).
export PATH="$HOME/.poetry/bin:$PATH"  # Scripts installed by poetry (Python).
export PATH="$HOME/.cargo/bin:$PATH"  # Binaries installed by cargo (Rust).
export PATH="$HOME/node_modules/.bin:$PATH"  # Scripts installed by NodeJS.
export PATH="$HOME/.go/bin:$PATH"  # Binaries installed by Go (GoLang).

MACOS="darwin*"
LINUX="linux-gnu"

if [[ $OSTYPE =~ $MACOS ]]; then
    export CLICOLORS=1
    export PATH="$HOME/Library/Python/3.7/bin:$PATH"  # Scripts installed by pip (Python) on macOS.
    export PATH="/usr/local/opt/llvm/bin:$PATH"  # LLVM and Clang binaries.
    export PATH="/usr/local/sbin:$PATH"

    [[ /usr/local/bin/kubectl ]] && source <(kubectl completion zsh)
fi

# Aliases

alias dud='du -hd1'
alias pyclean="find . \( -name \*.pyc -o -name \*.pyo -o -name __pycache__ \) -delete"
alias vim="nvim"
alias vimdiff="nvim -d"
alias jqr="jq --raw-input 'fromjson? | select(type == \"object\")'"
alias tmuxp="DISABLE_AUTO_TITLE='true' tmuxp"

if [[ $OSTYPE == $LINUX ]]; then
    alias open='mimeo'
    alias gvim="nvim-qt"
    alias add-sink-2="pactl load-module module-jack-sink client_name=pulse_sink_2 connect=yes"
fi

if [[ $OSTYPE =~ $MACOS ]]; then
    alias updatedb="sudo /usr/libexec/locate.updatedb"
    alias gvim="vimr"
    alias brew-upd="brew update; brew upgrade; brew cask upgrade; brew cleanup"
fi

# To customize prompt, run `p10k configure` or edit ~/.p10k.zsh.
[[ ! -f ~/.p10k.zsh ]] || source ~/.p10k.zsh
