let $VIMHOME=$HOME . "/.vim"
let $VIMBUNDLE=$VIMHOME . "/bundle"

" Create directories required by Vim configuration.
if !isdirectory($VIMHOME . "/backups")
    call mkdir($VIMHOME . "/backups", "p")
endif
if !isdirectory($VIMHOME . "/swap")
    call mkdir($VIMHOME . "/swap", "p")
endif


" {{{ OPTIONS

syntax on

set background=dark
set backspace=indent,eol,start
set backup
set backupdir=$VIMHOME/backups
set browsedir=buffer
set colorcolumn=80
"set completeopt=longest,menuone
set cursorcolumn
set cursorline
set directory=$VIMHOME/swap
set fileencodings=utf-8,windows-1251,iso-8859015,koi8-r,latin1
"set fillchars=
set foldmethod=marker
set formatoptions+=r  " Automatically insert current comment leader on Enter.
set hidden
set laststatus=2
set lazyredraw  " Speedup execution during macros and other untyped commands.
set listchars=tab:->,trail:-
set matchpairs+=<:>
set mouse=a
set mousemodel=popup_setpos
set nowrap
set number
set path+=**
set scrolloff=3
"set showfulltag
set spelllang=en_us,ru_yo,uk
set splitbelow
set splitright
" Set native status line as fallback from vim-airline.
set statusline=%f\ %m\ %r\ %y\ [%{&fileencoding}]\ [len\ %L:%p%%]
set statusline+=\ [pos\ %02l:%02c\ 0x%O]\ [chr\ %3b\ 0x%02B]\ [buf\ #%n]
set textwidth=79
set undodir=$VIMHOME/swap
set undofile
set updatetime=1000  " For more efficient Tagbar functioning
set virtualedit=all
set visualbell
set wildmenu

" Search
" ------

set hlsearch
set ignorecase
set incsearch
set nowrapscan
set smartcase

" Indent
" ------

set autoindent
set expandtab
set shiftwidth=4
set softtabstop=4
set tabstop=4

" Filetype settings
" -----------------

let c_comment_strings = 1
let c_curly_error = 1  " Highlight a missing `}` (may be slow).
let c_space_errors = 1  " Highlight extra white spaces.
let g:load_doxygen_syntax = 1
let g:tex_flavor = "latex"  " Consider .tex files as LaTeX instead of plainTeX.
let g:tex_indent_brace = 0  " Prevent overindentation for `]` and `}`.
let g:xml_syntax_folding = 1

" }}}
