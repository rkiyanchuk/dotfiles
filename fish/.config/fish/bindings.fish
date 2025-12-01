if status is-interactive; and type -q fzf; and type -q git
    function __git_select_ref_widget --description "wrapper for git_select_ref key binding"
        git rev-parse --git-dir >/dev/null 2>&1
        and git-select-ref
        commandline -f repaint
    end

    # Select git branch or tag
    bind ctrl-alt-b __git_select_ref_widget
end
