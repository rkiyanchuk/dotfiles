if status is-interactive
  function ls --wraps='ls --color=auto --group-directories-first' --description 'alias ls=ls --color=auto --group-directories-first'
      if type -q eza
        command eza --group-directories-first --group $argv;
      else
        command ls --color=auto --group-directories-first $argv;
      end
  end
end