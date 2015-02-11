XPTemplate priority=personal

let s:f = g:XPTfuncs()

" Set your python version with this variable in .vimrc or your own python
" snippet:
" XPTvar $PYTHON_EXC    /usr/bin/python
XPTvar $PYTHON_EXC    /usr/bin/env python

XPTvar $PYTHON_DOC_MARK '"""'

XPTvar $PYTHON_EXP_SYM ' as '

XPTvar $SPfun      ''
XPTvar $SParg      ''
XPTvar $SPcmd      ' '
XPTvar $SPop       ' '

XPTvar $CS    #
XPTvar $CL    '"""'
XPTvar $CM    ''
XPTvar $CR    '"""'

XPT docstr wrap	" $CL$CM ..
`$CL^`$CM^`$CM^`cursor^
`$CR^
..XPT
