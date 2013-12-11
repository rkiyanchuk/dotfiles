Vim version 7.3 has invalid spell file for english that causes the followin error on attempt to fun spell check:

    Error detected while processing /home/zoresvit/.vim/spell/uk.utf-8.spl:
    E763: Word characters differ between spell files 

To fix the error an upated Vim runtime file `en.utf-8.spl` needs to be downloaded and saved to user's `.vim/spell/` directory. With the next version of Vim this file may be removed.
