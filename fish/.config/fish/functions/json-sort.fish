if status is-interactive; and type -q python3
  function json-sort --description 'sort JSON object keys in place'
    if test (count $argv) -eq 0
      echo "usage: json-sort FILE..." >&2
      return 1
    end
    for file in $argv
      python3 -m json.tool --sort-keys --indent 2 $file $file
    end
  end
end
