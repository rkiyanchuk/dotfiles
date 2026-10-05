if status is-interactive; and type -q fzf; and type -q zmx
  function zmx-select --description "attach to a zmx session, or create one"
    # Browse (enter attaches, ctrl-x kills) and create (ctrl-a, enter creates,
    # esc backs out) modes in one fzf; see __zmx_select_transform. Create mode
    # suggests __zmx_default_name and is where the picker opens with no sessions.
    set -l sessions (__zmx_sessions)
    set -l prompt 'session ❯ '
    set -l label ' ctrl-a: new · ctrl-x: kill '
    set -l query
    if test (count $sessions) -eq 0
      set prompt (printf '\033[1;31mnew session ❯ \033[0m')
      set label ' enter: create · esc: cancel '
      set query (__zmx_default_name)
    end

    # string join emits nothing for an empty list, so fzf gets zero records.
    # --with-shell skips loading config.fish for every fzf action.
    set -l name (string join \n -- $sessions | fzf --ansi --no-multi \
      --with-shell='sh -c' \
      --layout=reverse --height=80% \
      --prompt=$prompt --query="$query" \
      --accept-nth=1 \
      --list-border=rounded \
      --list-label=$label \
      --list-label-pos='-3:bottom' \
      --color='list-label:dim' \
      --preview='zmx history {1} 2>/dev/null' \
      --preview-window='down,60%,follow' \
      --bind="ctrl-a:transform(fish -c '__zmx_select_transform ctrl-a')" \
      --bind="enter:transform(fish -c '__zmx_select_transform enter')" \
      --bind="ctrl-x:transform(fish -c '__zmx_select_transform ctrl-x')" \
      --bind="esc:transform:case \"\$FZF_PROMPT\" in *'new session'*) fish -c '__zmx_select_transform esc' ;; *) echo abort ;; esac")
    set name (string trim -- $name)
    if test -n "$name"
      zmx attach $name
    end
  end
end
