
function __git_select_ref_widget --description "wrapper for git_select_ref key binding"
    git-select-ref
    commandline -f repaint
end


# Select git branch or tag
bind ctrl-alt-b __git_select_ref_widget