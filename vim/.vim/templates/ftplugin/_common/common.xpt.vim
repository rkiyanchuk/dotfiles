" AUTHOR: Ruslan Kiianchuk <ruslan.kiianchuk@gmail.com>
" Personal customizations to XPTemplates.

XPTemplate priority=personal

let s:f = g:XPTfuncs()

XPTvar $author  Zoresvit
XPTvar $email   ruslan.kiianchuk@gmail.com

XPTvar $SPcmd      ' '
XPTvar $SPfun      ''
XPTvar $SParg      ''
XPTvar $SPop       ''

XPTvar $BRif     ' '
XPTvar $BRel     ' '
XPTvar $BRloop   ' '
XPTvar $BRstc    \n
XPTvar $BRfun    ' '


" FUNCTIONS
" =========

fun! s:f.headerSymbol(...)
    " Prepare string for header guard based on current filename.
    let h = expand('%:t')
    let h = substitute(h, '\.', '_', 'g') " replace . with _
    let h = substitute(h, '.', '\U\0', 'g') " make all characters upper case
    return h.'_'
endfunction

" LICENSES
" ========

XPT license_gpl3single " GNU GPL V3 License for single file program
`$CL^   This program is free software: you can redistribute it and/or modify
`$CM^   it under the terms of the GNU General Public License as published by
`$CM^   the Free Software Foundation, either version 3 of the License, or
`$CM^   (at your option) any later version.
`$CM^
`$CM^   This program is distributed in the hope that it will be useful,
`$CM^   but WITHOUT ANY WARRANTY; without even the implied warranty of
`$CM^   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
`$CM^   GNU General Public License for more details.
`$CM^
`$CM^   You should have received a copy of the GNU General Public License
`$CM^   along with this program.  If not, see <http://www.gnu.org/licenses/>.
`$CR^
..XPT

XPT license_gpl3multiple " GNU GPL V3 License for multiple files program
`$CL^   This file is part of `Program^.
`$CM^
`$CM^   `Program^ is free software: you can redistribute it and/or modify
`$CM^   it under the terms of the GNU General Public License as published by
`$CM^   the Free Software Foundation, either version 3 of the License, or
`$CM^   (at your option) any later version.
`$CM^
`$CM^   `Program^ is distributed in the hope that it will be useful,
`$CM^   but WITHOUT ANY WARRANTY; without even the implied warranty of
`$CM^   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
`$CM^   GNU General Public License for more details.
`$CM^
`$CM^   You should have received a copy of the GNU General Public License
`$CM^   along with `Program^.  If not, see <http://www.gnu.org/licenses/>.
`$CR^
..XPT

" TEMPLATES
" =========

" Doxygen comment.
XPT dox alias=_d_commentDoc


XPT guard wrap	" #ifndef .. #define ..
XSET symbol=headerSymbol()
#ifndef `symbol^
#define `symbol^

`cursor^

#endif  `$CL^ `symbol^ `$CR^
..XPT
