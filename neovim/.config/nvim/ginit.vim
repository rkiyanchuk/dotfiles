call rpcnotify(1, 'Gui', 'Option', 'Tabline', 0)
call rpcnotify(1, 'Gui', 'Option', 'Popupmenu', 0)

if has("mac")
    Guifont! Iosevka Nerd Font Mono:l:h16
else
    Guifont! Iosevka:l:h14
endif
