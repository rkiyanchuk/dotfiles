if status is-interactive; and type -q fzf; and type -q git
    function __git_select_ref_widget --description "wrapper for git_select_ref key binding"
        git rev-parse --git-dir >/dev/null 2>&1
        and git-select-ref
        commandline -f repaint
    end

    # Select git branch or tag
    bind ctrl-g __git_select_ref_widget

    function __git_select_worktree_widget --description "wrapper for git_select_worktree key binding"
        git rev-parse --git-dir >/dev/null 2>&1
        and git-select-worktree
        commandline -f repaint
    end

    # Select git worktree
    bind ctrl-alt-w __git_select_worktree_widget
end

if status is-interactive; and type -q zmx
    function __zmx_attach_widget --description "wrapper for zmx attach key binding"
        # Only reachable with ZMX_NO_DETACH_KEY; zmx otherwise eats ctrl-\.
        set -q ZMX_SESSION; and return
        zmx attach (__zmx_default_name)
        commandline -f repaint
    end

    # New session named after the git root or current dir (detach inside one)
    bind ctrl-\\ __zmx_attach_widget
end

if status is-interactive; and type -q fzf; and type -q zmx
    function __zmx_select_widget --description "wrapper for zmx-select key binding"
        # Picking a session from inside one would switch or nest it.
        set -q ZMX_SESSION; and return
        zmx-select
        commandline -f repaint
    end

    # Pick or create a session. Kitty keyboard protocol reports ctrl-shift-\
    # either as shift+\ or as the shifted character.
    bind ctrl-shift-\\ __zmx_select_widget
    bind ctrl-\| __zmx_select_widget
end
