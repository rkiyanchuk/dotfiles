"
" AUTHOR: Ruslan Kiianchuk <ruslan.kiianchuk@gmail.com>
"
" For better understanding of each uncommented setting see ":help '<item>'".


if has('unix')
    let $VIMHOME=$HOME . "/.vim"
    let $VIMBUNDLE=$VIMHOME . "/bundle"
endif

if has('win32') || has('win64')
    let $VIMHOME=getcwd()
    let $VIMHOME=$VIMHOME . '\Data\settings\.vim\'
    behave mswin
endif


" BUILTIN OPTIONS 
" =============== {{{ 

" Appearance
" ----------

set background=dark
set colorcolumn=80
set cursorcolumn
set cursorline
set fillchars=
set laststatus=2
set number
set statusline=%f\ %m\ %r\ %y\ [%{&fileencoding}]\ [len\ %L:%p%%]
set statusline+=\ [pos\ %02l:%02c\ 0x%O]\ [chr\ %3b\ 0x%02B]\ [buf\ #%n]

" Behavior
" --------

set nocompatible  " Ensure Vi improved features are enabled.
syntax on
set backspace=indent,eol,start
set backup
set backupdir=$VIMHOME/backups
set browsedir=buffer
set completeopt=menuone,preview
set directory=$VIMHOME/swap
set fileencodings=utf-8,windows-1251,iso-8859015,koi8-r,latin1
set foldmethod=marker
set hidden
set lazyredraw  " Speedup execution during macros and other untyped commands.
set listchars=tab:->,trail:-
set matchpairs+=<:>
set nowrap
set path+=.,,**
set scrolloff=3
set showfulltag
set spelllang=en,ru_yo,uk
set splitbelow
set splitright
set textwidth=80
set timeoutlen=500
set undofile
set undodir=$VIMHOME/backups
set undolevels=2048
set virtualedit=all
set visualbell
set wildmenu
" Close omni-completion preview window when entering or leaving insert mode.
autocmd CursorMovedI * if pumvisible() == 0|pclose|endif
autocmd InsertLeave * if pumvisible() == 0|pclose|endif
command! -bar Hex call ToggleHex()
" Create missing directories
if !isdirectory($VIMHOME . "/backups")
    call mkdir($VIMHOME . "/backups", "p")
endif
if !isdirectory($VIMHOME . "/swap")
    call mkdir($VIMHOME . "/swap", "p")
endif

" Search
" ~~~~~~

set hlsearch
set ignorecase
set incsearch
set nowrapscan
set smartcase

" Indent
" ~~~~~~

set autoindent
set expandtab
set shiftwidth=4
set softtabstop=4
set tabstop=4

" Filetype
" --------
let c_comment_strings=1
let c_curly_error=1  " Highlight a missing `}` (may be slow).
let c_space_errors=1  " Highlight extra white spaces.
let c_no_tab_space_error=1

let g:load_doxygen_syntax=1 " Load up the doxygen syntax

let g:tex_flavor="latex"  " Set `tex` filetype for *.tex extension.
let g:tex_indent_brace=0
" }}}


" MAPPINGS
" ======== {{{

" Default <leader> is \ (backslash). It may be redefined:
"let mapleader="\"
" Comma is followed by a space most of the time.
inoremap , ,<SPACE>
" Resource configuration editing.
nmap <silent> <leader>v :split $MYVIMRC<CR>
nmap <silent> <leader>g :split $MYGVIMRC<CR>
nmap <silent> <leader>V :source $MYVIMRC<CR>
nmap <silent> <leader>G :source $MYGVIMRC<CR>
" Restore last session.
nmap <leader>rs :call LoadSession()<CR>
" Control buffers.
nmap <silent> <leader>bb :buffers<CR>
nmap <silent> <leader>bl :b#<CR>
nmap <silent> <leader>bn :bnext<CR>
nmap <silent> <leader>bp :bprevious<CR>
nmap <silent> <leader>bd :bdelete<CR>
" Control quickfix window.
nmap <silent> <leader>co :copen<CR>
nmap <silent> <leader>cc :cclose<CR>
" Navigate compile errors in quickfix window.
nmap <silent> <leader>en :cn<CR>
nmap <silent> <leader>ep :cp<CR>
" Toggle spell check.
nmap <leader>s :set spell!<CR>
" Fix spelling by choosing first match from suggestions.
imap <silent> <leader>sf <ESC>1z=ea 
nmap <silent> <leader>sf <ESC>1z=e
" Map the placeholder <+ +> navigation
nnoremap <silent> <C-j> /<+.\{-1,}+><CR>c/+>/e<CR>
inoremap <silent> <C-j> <ESC>/<+.\{-1,}+><CR>c/+>/e<CR>
" Make the current file executable.
nmap <leader>x :w<CR>:!chmod 755 %<CR>:e<CR><CR>
" Remove trailing spaces (http://vim.wikia.com/wiki/Remove_unwanted_spaces).
nnoremap <leader>rts :let _s=@/<Bar>:%s/\s\+$//e<Bar>:let @/=_s<Bar>:nohl<CR>
" Enable russian alternate layout.
inoremap <leader>ru <ESC>:set keymap=russian-jcukenwin<CR>a
nnoremap <leader>ru :set keymap=russian-jcukenwin<CR>
" Enable ukrainian alternate layout.
inoremap <leader>uk <ESC>:set keymap=ukrainian-jcuken<CR>a
nnoremap <leader>uk :set keymap=ukrainian-jcuken<CR>
" Enable hebrew alternate layout.
inoremap <leader>he <ESC>:set keymap=hebrew_utf-8<CR>a
nnoremap <leader>he :set keymap=hebrew_utf-8<CR>
" }}}


" AUTOCOMMANDS
" ============ {{{

augroup TEXT
    au!
    " Insert predefined boilerplate on file creation.
    au BufNewFile * silent! 0r $VIMHOME/templates/boilerplates/%:e.tpl
    " Enable quickfix window height adjustment.
    au FileType qf call AdjustWindowHeight(3, 6)
    " Automatically save session on exit.
    au VimLeave * call SaveSession()
    " Navigate line by line through wrapped text (skip wrapped lines).
    au BufReadPre * imap <UP> <ESC>gka
    au BufReadPre * imap <DOWN> <ESC>gja
    " Navigate row by row through wrapped text.
    au BufReadPre * nmap k gk
    au BufReadPre * nmap j gj
    " Correct filetype detection for *.md files.
    au BufRead,BufNewFile *.md set filetype=markdown
augroup END

augroup PROGRAMMING
    au FileType c,cpp,h,tex map <F5> :call Compile()<CR>
    au FileType c,cpp,h set cindent
    au FileType c,cpp,h set cinoptions=h3,l1,g1,t0,i4,+4,(0,w1,W4
    au BufRead,BufNewFile *.html set shiftwidth=2
    au BufRead,BufNewFile *.html set softtabstop=2
    au BufRead,BufNewFile *.html set tabstop=2
    " Check current file for correspondence to Google Style Guide with cpplint tool
    au FileType c,cpp nmap <F8> :set makeprg=$VIMHOME/utils/cpplint.py\ %<CR>:make<CR>:copen<CR><CR>
    " Correctly set filetype for configuration files.
    au BufRead,BufNewFile *.conf set filetype=cfg
    " Sage specific configuration.
    au BufRead,BufNewFile *.sage set filetype=python
    " LaTeX
    let $TEXTARGET=substitute(@%, ".tex", ".pdf", "")
    au FileType tex set makeprg=make\ -f\ $VIMHOME/utils/Makefile_tex\ TARGET=%
    au FileType tex map <F5> :make -B <CR>
    " Binary files
    au BufWritePre * if exists("b:editHex") && b:editHex==1 | call ToggleHex() | endif
    au BufWritePost * if exists("b:editHex") && b:editHex==0 | call ToggleHex() | endif
    " Haskell
    au BufRead,BufNewFile .xmobarrc set filetype=haskell
augroup END
" }}}


" FUNCTIONS
" ========= {{{

function! AdjustWindowHeight(minheight, maxheight)
    " Adjust QuickFix window height according to the number of data lines.
    exe max([min([line("$"), a:maxheight]), a:minheight]) . "wincmd _"
endfunction

function! Compile()
    " Recursively look for Makefile or SConstruct and perform build.
    let origcurdir = getcwd()
    let curdir     = origcurdir
    while curdir != $HOME
        if filereadable("Makefile")
            break
        elseif filereadable("SConstruct")
            break
        endif
        cd ..
        let curdir= getcwd()
    endwhile
    if filereadable('Makefile')
        set makeprg=make
    elseif filereadable('SConstruct')
        set makeprg=scons
    else
        set makeprg=make
    endif
    echo "now building..."
    silent w
    make
    echo "build finished"
endfunction

function! LoadSession()
    " Load last vim session
    if argc() == 0
        execute 'source $VIMHOME/session.vim'
    endif
endfunction

function! SaveSession()
    " Save current vim session.
    execute 'mksession! $VIMHOME/session.vim'
endfunction

function! ToggleHex()
    " Helper function for processing binary files via hex editor.
    let l:modified=&mod
    let l:oldreadonly=&readonly
    let &readonly=0
    let l:oldmodifiable=&modifiable
    let &modifiable=1
    if !exists("b:editHex") || !b:editHex
        " save old options
        let b:oldft=&ft
        let b:oldbin=&bin
        " set new options
        setlocal binary " make sure it overrides any textwidth, etc.
        let &ft="xxd"
        " set status
        let b:editHex=1
        " switch to hex editor
        %!xxd
    else
        " restore old options
        let &ft=b:oldft
        if !b:oldbin
            setlocal nobinary
        endif
        " set status
        let b:editHex=0
        " return to normal editing
        %!xxd -r
    endif
    " restore values for modified and read only state
    let &mod=l:modified
    let &readonly=l:oldreadonly
    let &modifiable=l:oldmodifiable
endfunction
" }}}


" VUNDLE PLUGINS
" ============== {{{

filetype off  " Filetype recognition must be disabled for Vundle setup.
set rtp+=$VIMBUNDLE/vundle/
call vundle#rc()

" Update Vundle itself.
Bundle 'gmarik/vundle'
" Solarized colorscheme.
Bundle 'altercation/vim-colors-solarized'
" Insert predefined text templates with placeholders.
Bundle 'drmingdrmer/xptemplate'
" File browser.
Bundle 'scrooloose/nerdtree'
" Gitk for vim (depends on fugitive).
Bundle 'gregsexton/gitv'
Bundle 'tpope/vim-fugitive'
" Show project structure.
Bundle 'majutsushi/tagbar'
" C/C++ autocompletion using clang.
Bundle 'Rip-Rip/clang_complete'
" Surround text with tags.
Bundle 'tpope/vim-surround'
" Intuitive fuzzy files opening.
" Bundle 'wincent/Command-T'
Bundle 'kien/ctrlp.vim'

" Plugins for Python development.
Bundle 'davidhalter/jedi-vim'
Bundle 'nvie/vim-flake8'
" Enables advanced unit test support. Install dependencies first:
" $ pip install nose nose_machineout vim_bridge
Bundle 'nvie/vim-pyunit'
" python-mode is for syntax and highlighting (completion done by jedi-vim).
Bundle 'klen/python-mode'
" Enhanced statusline
Bundle 'bling/vim-airline'
" Jinja & HTML syntax
Bundle 'Glench/Vim-Jinja2-Syntax'

" Turn on filetype recognition, load filetype specific plugins and indents.
filetype plugin indent on

" Clang_complete
" --------------

if has('unix')
    " clang_complete
    " --------------
    let g:clang_use_library=1
    let g:clang_hl_errors=1
    let g:clang_complete_copen=1
    let g:clang_periodic_quickfix=0
    autocmd Filetype c,cpp,cxx,h,hxx autocmd BufWritePre <buffer> :call g:ClangUpdateQuickFix()
endif

" CtrlP
" -----

let g:ctrlp_map = '<C-p>'

" NERDTree
" --------

imap <F2> :NERDTreeToggle<CR>
nmap <F2> :NERDTreeToggle<CR>

" Python-mode
" -----------

let g:pymode_lint = 0
let g:pymode_rope = 0
let g:pymode_folding = 0
let g:pymode_motion = 1  " Enable python-specific motions.
let g:pymode_indent = 1
let g:pymode_utils_whitespaces = 1
let g:pymode_breakpoint = 1
let g:pymode_breakpoint_key = '<leader>pb'
let g:pymode_syntax = 1
let g:pymode_syntax_all = 1


" Solarized
" ---------

if isdirectory($VIMBUNDLE . "/vim-colors-solarized")
    let g:solarized_bold=0
    let g:solarized_underline=0
    let g:solarized_italic=0
    colorscheme solarized
endif

" Tagbar
" ------

imap <F3> :TagbarToggle<CR>
nmap <F3> :TagbarToggle<CR>

" XPTemplate
" ----------

let g:xptemplate_brace_complete='([{<'
set rtp+=$VIMHOME/templates/
" }}}
