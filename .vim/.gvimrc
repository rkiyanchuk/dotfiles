"
" AUTHOR: Ruslan Kiianchuk <ruslan.kiianchuk@gmail.com>
"
" For better understanding of each uncommented setting see ":help '<item>'".


" Set up the gui cursor look nice
set guicursor=n-v-c:block-Cursor-blinkon0
set guicursor+=ve:ver35-Cursor
set guicursor+=o:hor50-Cursor
set guicursor+=i-ci:ver25-Cursor
set guicursor+=r-cr:hor20-Cursor
set guicursor+=sm:block-Cursor-blinkwait175-blinkoff150-blinkon175
" Disable visual bell flashing.
set t_vb=

if has('unix')
    set guifont=Liberation\ Mono\ 12
    set guioptions=ca  " Get rid of toolbars and menues
endif

if has('win32') || has('win64')
    set guifont=Consolas:h12
endif
