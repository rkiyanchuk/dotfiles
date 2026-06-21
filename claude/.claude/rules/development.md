---
paths:
    - "**/*.c"
    - "**/*.h"
    - "**/*.cc"
    - "**/*.cpp"
    - "**/*.cxx"
    - "**/*.hpp"
    - "**/*.cs"
    - "**/*.go"
    - "**/*.rs"
    - "**/*.java"
    - "**/*.kt"
    - "**/*.kts"
    - "**/*.scala"
    - "**/*.swift"
    - "**/*.m"
    - "**/*.mm"
    - "**/*.py"
    - "**/*.rb"
    - "**/*.php"
    - "**/*.pl"
    - "**/*.pm"
    - "**/*.lua"
    - "**/*.js"
    - "**/*.jsx"
    - "**/*.mjs"
    - "**/*.cjs"
    - "**/*.ts"
    - "**/*.tsx"
    - "**/*.vue"
    - "**/*.svelte"
    - "**/*.dart"
    - "**/*.ex"
    - "**/*.exs"
    - "**/*.erl"
    - "**/*.hrl"
    - "**/*.clj"
    - "**/*.cljs"
    - "**/*.hs"
    - "**/*.ml"
    - "**/*.mli"
    - "**/*.fs"
    - "**/*.fsx"
    - "**/*.jl"
    - "**/*.r"
    - "**/*.R"
    - "**/*.zig"
    - "**/*.nim"
    - "**/*.cr"
    - "**/*.sh"
    - "**/*.bash"
    - "**/*.zsh"
    - "**/*.fish"
    - "**/*.ps1"
    - "**/*.css"
    - "**/*.scss"
    - "**/*.sass"
    - "**/*.less"
---

# Development

- Follow YAGNI principle.

## Decision order

Before writing code, walk down this list and stop at the first match:

1. Does this need to exist? → no: skip it (YAGNI)
2. Stdlib does it? → use it
3. Native platform feature? → use it
4. Installed dependency? → use it
5. One line? → one line
6. Only then: the minimum that works
