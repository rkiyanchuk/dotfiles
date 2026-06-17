if status is-interactive; and type -q claude
  function claude --wraps=claude --description 'Override notification channel per terminal (Zed has no native channel)'
    # `claude -R [args]` -> interactive session picker (claude-resume)
    if test (count $argv) -gt 0; and test "$argv[1]" = -R
      claude-resume $argv[2..]
      return
    end
    switch "$TERM_PROGRAM"
      case zed
        command claude --settings '{"preferredNotifChannel":"terminal_bell"}' $argv
      case '*'
        command claude $argv
    end
  end
end
