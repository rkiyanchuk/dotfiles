function __zmx_default_name --description "first free zmx session name for the git root or current dir"
  # Repo or dir basename, then -2, -3, ...; zmx names a socket file after it.
  set -l root (git rev-parse --show-toplevel 2>/dev/null); or set root $PWD
  set -l prefix (string replace -ra '[^A-Za-z0-9._-]+' - -- (path basename -- $root))
  set -l taken (zmx ls --short 2>/dev/null)
  set -l name $prefix
  set -l n 1
  while contains -- $name $taken
    set n (math $n + 1)
    set name $prefix-$n
  end
  echo $name
end
