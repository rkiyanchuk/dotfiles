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

if status is-interactive; and type -q fzf; and type -q zmx
    function __zmx_select_widget --description "wrapper for zmx-select key binding"
        # Inside a session the zmx client consumes ctrl-\ as detach before fish
        # sees it; the guard only matters with ZMX_NO_DETACH_KEY set, where a
        # picker here would attach a nested session.
        set -q ZMX_SESSION; and return
        zmx-select
        commandline -f repaint
    end

    # Select or create zmx session (detach when inside one)
    bind ctrl-\\ __zmx_select_widget
end
