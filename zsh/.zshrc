# Path to your oh-my-zsh installation.
export ZSH="${HOME}/.oh-my-zsh"

export HISTFILESIZE=
export HISTSIZE=
export HIST_STAMPS="yyyy-mm-dd"
setopt HIST_FIND_NO_DUPS
setopt HIST_IGNORE_ALL_DUPS
setopt INC_APPEND_HISTORY

# Homebrew completion
if type brew &>/dev/null; then
  FPATH=$(brew --prefix)/share/zsh/site-functions:$FPATH
  autoload -Uz compinit
  compinit
fi

# Which plugins would you like to load?
# Standard plugins can be found in ~/.oh-my-zsh/plugins/*
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
plugins=(
    colored-man-pages
    docker
    docker-compose
)

source $ZSH/oh-my-zsh.sh

bindkey "^[F" emacs-forward-word
bindkey "^[[1;5C" emacs-forward-word
bindkey "^[f" emacs-forward-word

# USER SETTINGS

export EDITOR="nvim"
export GPG_TTY=$TTY
export GOPATH="$HOME/.go"

export GLAMOUR_STYLE="dark"  # GitHub CLI Markdown rendering style.

export PATH="/usr/local/sbin:$PATH"
export PATH="$HOME/.local/bin:$PATH"  # Scripts installed by pip (Python).
export PATH="$HOME/.poetry/bin:$PATH"  # Scripts installed by poetry (Python).
export PATH="$HOME/.cargo/bin:$PATH"  # Binaries installed by cargo (Rust).
export PATH="$HOME/node_modules/.bin:$PATH"  # Scripts installed by NodeJS.
export PATH="/Applications/IntelliJ IDEA.app/Contents/MacOS:$PATH"  # Add IntelliJ IDEA scripts.
export PATH="$GOPATH/bin:$PATH"  # Binaries installed by Go (GoLang).

MACOS="darwin*"
LINUX="linux-gnu"

if [[ $OSTYPE =~ $MACOS ]]; then
    export CLICOLORS=1
    export PATH="$HOME/Library/Python/3.8/bin:$PATH"  # Scripts installed by pip (Python) on macOS.
    export PATH="/usr/local/opt/llvm/bin:$PATH"  # LLVM and Clang binaries.
    export PATH="/usr/local/sbin:$PATH"
fi

# Aliases

alias dud='du -hd1'
alias pyclean="find . \( -name \*.pyc -o -name \*.pyo -o -name __pycache__ \) -delete"
alias vim="nvim"
alias vimdiff="nvim -d"

if [[ $OSTYPE == $LINUX ]]; then
    alias open='xdg-open'
fi

if [[ $OSTYPE =~ $MACOS ]]; then
    alias updatedb="sudo /usr/libexec/locate.updatedb"
fi

# Use `jq` with both JSON and non-JSON lines, dropping non-JSON.
alias jqq="jq -R 'fromjson? | select(type == \"object\")'"
# Use `jq` with both JSON and non-JSON lines, preserving non-JSON.
alias jqr='jq -R -r ". as \$line | try fromjson catch \$line"'

function urldecode { python3 -c "import urllib.parse as url; print(url.unquote('$1'))" }

# checkout git branch/tag, with a preview showing the commits between the tag/branch and HEAD
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

eval "$(starship init zsh)"
