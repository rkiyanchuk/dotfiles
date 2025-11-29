if status is-interactive; and type -q python
  function http-serve --wraps='python -m http.server' --description 'run Python HTTP server in current dir'
    python -m http.server $argv;
  end
end