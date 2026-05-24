if status is-interactive; and type -q claude
  function claude --wraps=claude --description 'Override notification channel per terminal (Zed has no native channel)'
    switch "$TERM_PROGRAM"
      case zed
        command claude --settings '{"preferredNotifChannel":"terminal_bell"}' $argv
      case '*'
        command claude $argv
    end
  end
end
