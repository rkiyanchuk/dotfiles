if status is-interactive; and type -q find
  function pyclean --description "delete all temporary Python files"
      find . \( -name \*.pyc -o -name \*.pyo -o -name __pycache__ \) -delete
  end
end