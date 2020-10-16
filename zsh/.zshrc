# Enable Powerlevel10k instant prompt. Should stay close to the top of ~/.zshrc.
# Initialization code that may require console input (password prompts, [y/n]
# confirmations, etc.) must go above this block; everything else may go below.
if [[ -r "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh" ]]; then
  source "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh"
fi

# Path to your oh-my-zsh installation.
export ZSH="${HOME}/.oh-my-zsh"

ZSH_THEME="powerlevel10k/powerlevel10k"

HIST_STAMPS="yyyy-mm-dd"

# Which plugins would you like to load?
# Standard plugins can be found in ~/.oh-my-zsh/plugins/*
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
plugins=(
    colored-man-pages
    # git clone https://github.com/qoomon/zsh-lazyload $ZSH_CUSTOM/plugins/zsh-lazyload
    zsh-lazyload
    docker
    docker-compose
)

source $ZSH/oh-my-zsh.sh

bindkey "^[F" emacs-forward-word
bindkey "^[[1;5C" emacs-forward-word
bindkey "^[f" emacs-forward-word

# USER SETTINGS

export EDITOR="nvim"
export GPG_TTY=$(tty)
export GOPATH="$HOME/.go"

export GLAMOUR_STYLE="dark"  # GitHub CLI Markdown rendering style.

export PATH="/usr/local/sbin:$PATH"
export PATH="$HOME/.local/bin:$PATH"  # Scripts installed by pip (Python).
export PATH="$HOME/.poetry/bin:$PATH"  # Scripts installed by poetry (Python).
export PATH="$HOME/.cargo/bin:$PATH"  # Binaries installed by cargo (Rust).
export PATH="$HOME/node_modules/.bin:$PATH"  # Scripts installed by NodeJS.
export PATH="$GOPATH/bin:$PATH"  # Binaries installed by Go (GoLang).

MACOS="darwin*"
LINUX="linux-gnu"

if [[ $OSTYPE =~ $MACOS ]]; then
    export CLICOLORS=1
    export PATH="$HOME/Library/Python/3.7/bin:$PATH"  # Scripts installed by pip (Python) on macOS.
    export PATH="/usr/local/opt/llvm/bin:$PATH"  # LLVM and Clang binaries.
    export PATH="/usr/local/sbin:$PATH"
fi

# Aliases

alias dud='du -hd1'
alias pyclean="find . \( -name \*.pyc -o -name \*.pyo -o -name __pycache__ \) -delete"
alias vim="nvim"
alias vimdiff="nvim -d"
alias tmuxp="DISABLE_AUTO_TITLE='true' tmuxp"

# Use `jq` with both JSON and non-JSON lines.
function jqr { jq -R -r "${1:-.} as \$line | try fromjson catch \$line" }

if [[ $OSTYPE == $LINUX ]]; then
    alias open='xdg-open'
    alias gvim="nvim-qt"
    alias add-sink-2="pactl load-module module-jack-sink client_name=pulse_sink_2 connect=yes"
fi

if [[ $OSTYPE =~ $MACOS ]]; then
    alias updatedb="sudo /usr/libexec/locate.updatedb"
    alias gvim="vimr --cur-env"
    alias brew-upd="brew update; brew upgrade; brew cleanup"

fi

# fco_preview - checkout git branch/tag, with a preview showing the commits between the tag/branch and HEAD
fzg() {
  local tags branches target
  branches=$(
    git --no-pager branch $1 \
      --format="%1B[0;34;1mbranch%09%1B[m%(refname:short)" \
    | sed '/^$/d') || return
  tags=$(
    git --no-pager tag | awk '{print "\x1b[35;1mtag\x1b[m\t" $1}') || return
  target=$(
    (echo "$branches"; echo "$tags") |
    fzf --no-hscroll --no-multi -n 2 \
        --ansi --preview="git --no-pager log -150 --pretty=format:%s '..{2}'") || return
  branch=$(awk '{print $2}' <<<"$target")
  if [[ -n ${1} ]]; then
      branch=${branch//origin\//}
  fi
  git checkout "${branch}"
}


# To customize prompt, run `p10k configure` or edit ~/.p10k.zsh.
[[ ! -f ~/.p10k.zsh ]] || source ~/.p10k.zsh
