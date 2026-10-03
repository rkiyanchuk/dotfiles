if status is-interactive; and type -q fzf; and type -q zmx
  function zmx-select --description "attach to a zmx session, or create one"
    # Two modes in one fzf: browse (enter attaches, ctrl-x kills) and create
    # (ctrl-a switches in, type a name, enter creates, esc backs out). With no
    # sessions the picker opens straight in create mode. Mode-sensitive keys
    # route through __zmx_select_transform.
    set -l sessions (zmx ls --short 2>/dev/null)
    set -l prompt 'session ❯ '
    set -l label ' ctrl-a: new · ctrl-x: kill '
    if test (count $sessions) -eq 0
      set prompt (printf '\033[1;31mnew session ❯ \033[0m')
      set label ' enter: create · esc: cancel '
    end

    # string join, unlike printf, emits nothing for an empty list, so fzf sees
    # zero records instead of one blank one.
    #
    # --with-shell: fzf otherwise runs every action through $SHELL (fish), which
    # loads config.fish before the explicit `fish -c` does it again. Browse-mode
    # esc is answered by sh alone so quitting is as fast as a plain fzf.
    set -l name (string join \n -- $sessions | fzf --ansi --no-multi \
      --with-shell='sh -c' \
      --layout=reverse --height=50% \
      --prompt=$prompt \
      --list-border=rounded \
      --list-label=$label \
      --list-label-pos='-3:bottom' \
      --color='list-label:dim' \
      --preview='zmx history {} 2>/dev/null' \
      --preview-window='follow' \
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
