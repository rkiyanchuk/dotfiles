" Set up the GUI cursor look nice
set guicursor=n-v-c:block-Cursor-blinkon0
set guicursor+=ve:ver35-Cursor
set guicursor+=o:hor50-Cursor
set guicursor+=i-ci:ver25-Cursor
set guicursor+=r-cr:hor20-Cursor
set guicursor+=sm:block-Cursor-blinkwait175-blinkoff150-blinkon175

set linespace=2  " Make underscore visible for some fonts.
set guioptions=ca  " Remove toolbars and menues.

" Disable visual bell flashing.
set t_vb=


if has("gui_macvim")
    set guifont=DejaVu\ Sans\ Mono\ for\ Powerline:h14
else
    set guifont=DejaVu\ Sans\ Mono\ 14
endif
