function __zmx_sessions --description "list zmx sessions as aligned name, cwd, process rows"
  # pid is the session's shell; its tty's foreground process group is what runs.
  set -l home_re '^'(string escape --style=regex -- $HOME)'(?=/|$)'
  for line in (zmx list 2>/dev/null)
    set -l name (string match -rg '\bname=([^\t]+)' -- $line); or continue
    set -l dir (string match -rg '\tcwd=file://[^/]*([^\t]*)' -- $line | string unescape --style=url)
    set -l pid (string match -rg '\tpid=(\d+)' -- $line)
    set -l fg (ps -o tpgid= -p $pid 2>/dev/null | string trim)
    # Login shells report as -fish.
    set -l proc (ps -o comm= -p $fg 2>/dev/null | path basename | string trim -l -c -)
    printf '%s\t\e[36m%s\e[0m\t\e[35m%s\e[0m\n' $name (string replace -r -- $home_re '~' "$dir") "$proc"
  end | column -t -s \t
end
