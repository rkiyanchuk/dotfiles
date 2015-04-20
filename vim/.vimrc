" Ruslan Kiianchuk <ruslan.kiianchuk@gmail.com>


let $VIMHOME=$HOME . "/.vim"
let $VIMBUNDLE=$VIMHOME . "/bundle"

" Create dedicated directories.
if !isdirectory($VIMHOME . "/backups")
    call mkdir($VIMHOME . "/backups", "p")
endif
if !isdirectory($VIMHOME . "/swap")
    call mkdir($VIMHOME . "/swap", "p")
endif


" {{{ OPTIONS

set nocompatible  " Ensure Vi improved features are enabled.
syntax on

set background=dark
set backspace=indent,eol,start
set backup
set backupdir=$VIMHOME/backups
set browsedir=buffer
set colorcolumn=80
set complete+=k
set completeopt=menuone,preview
set cursorcolumn
set cursorline
set dictionary+=/usr/share/dict/words
set directory=$VIMHOME/swap
set fileencodings=utf-8,windows-1251,iso-8859015,koi8-r,latin1
set fillchars=
set foldmethod=marker
set formatoptions+=ro
set hidden
set laststatus=2
set lazyredraw  " Speedup execution during macros and other untyped commands.
set listchars=tab:->,trail:-
set matchpairs+=<:>
set mousemodel=popup
set nowrap
set number
set path+=.,,**
set scrolloff=3
set showfulltag
set spelllang=en,ru_yo,uk
set splitbelow
set splitright
set statusline=%f\ %m\ %r\ %y\ [%{&fileencoding}]\ [len\ %L:%p%%]
set statusline+=\ [pos\ %02l:%02c\ 0x%O]\ [chr\ %3b\ 0x%02B]\ [buf\ #%n]
set textwidth=79
set timeoutlen=500
set undodir=$VIMHOME/backups
set undofile
set undolevels=2048
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
let g:load_doxygen_syntax = 1  " Load doxygen syntax.
let g:tex_flavor = "latex"  " Consider .tex files as LaTeX instead of plainTeX.
let g:tex_indent_brace = 0  " Prevent overindentation for `]` and `}`.
let g:xml_syntax_folding = 1

" }}}


" {{{ AUTOCOMMANDS

" Close omni-completion preview window when entering or leaving insert mode.
autocmd CursorMovedI * if pumvisible() == 0|pclose|endif
autocmd InsertLeave * if pumvisible() == 0|pclose|endif

augroup TEXT
    " Auto commands for any text file.
    au!
    " Enable quickfix window height adjustment.
    au FileType qf call AdjustWindowHeight(3, 6)
    " Automatically save session on exit.
    au VimLeave * call SaveSession()
    " Navigate row by row through wrapped text.
    au BufReadPre * imap <UP> <ESC>gka
    au BufReadPre * imap <DOWN> <ESC>gja
    au BufReadPre * nmap k gk
    au BufReadPre * nmap j gj
augroup END

augroup CPP
    au!
    au FileType c,cpp,h map <F6> :call Compile()<CR>
    au FileType c,cpp,h set cindent
    au FileType c,cpp,h set cinoptions=h3,l1,g1,t0,i4,+4,(0,w1,W4
augroup END

augroup LATEX
    au FileType tex set makeprg=make\ -f\ $VIMHOME/Makefile_tex\ TARGET=%
    au FileType tex map <F6> :make -B <CR>
augroup END

augroup WEB
    au FileType html,yaml,xml set shiftwidth=2
    au FileType html,yaml,xml set softtabstop=2
    au FileType html,yaml,xml set tabstop=2

    au FileType xml setlocal foldmethod=syntax
    au FileType xml normal zR  " Open all folds by default.
augroup END

augroup MISC
    au!
    " Treat Xmobar config as Haskell file.
    au BufRead,BufNewFile .xmobarrc set filetype=haskell
    " Treat .conf files as .cfg.
    au BufRead,BufNewFile *.conf set filetype=cfg
augroup END

" }}}


" {{{ FUNCTIONS

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
    if filereadable('SConstruct')
        set makeprg=scons
    else
        set makeprg=make
    endif
    echo "Building..."
    silent w
    make
    echo "Build finished."
endfunction

function! LoadSession()
    " Load saved Vim session from file.
    if argc() == 0
        execute 'source $VIMHOME/session.vim'
    endif
endfunction

function! SaveSession()
    " Save current Vim session to file.
    execute 'mksession! $VIMHOME/session.vim'
endfunction

if !exists("*ReloadConfig")
    " Reload .vimrc and .gvimrc configuration files.
    function! ReloadConfig()
        source $MYVIMRC
        if has("gui_running")
          source $MYGVIMRC
        endif
    endfunction
endif

function! ShowSpaces(...)
  let @/='\v(\s+$)|( +\ze\t)'
  let oldhlsearch=&hlsearch
  if !a:0
    let &hlsearch=!&hlsearch
  else
    let &hlsearch=a:1
  end
  return oldhlsearch
endfunction

function! TrimSpaces() range
    let oldhlsearch=ShowSpaces(1)
    execute a:firstline.",".a:lastline."substitute ///ge"
    let &hlsearch=oldhlsearch
endfunction

" }}}


" {{{ COMMANDS

command! -bar -nargs=0 -range=% TrimSpaces <line1>,<line2>call TrimSpaces()

" Save restricted file opened without root permissions via sudo.
command! W :w !sudo tee %

"}}}


" {{{ MAPPINGS

" Default <leader> key is \ (backslash).
"let mapleader="\"
nmap <silent> <leader>V :split $MYVIMRC<CR>
nmap <silent> <leader>R :call ReloadConfig()<CR>
nmap <leader>ls :call LoadSession()<CR>
" List all buffers and load specified buffer number.
nnoremap <leader>bb :buffers<CR>:buffer<Space>
" Toggle spell check.
nmap <leader>s :set spell!<CR>
" Fix spelling by choosing first match from suggestions.
imap <silent> <leader>sf <ESC>1z=ea
nmap <silent> <leader>sf <ESC>1z=e
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


" {{{ PLUGINS

filetype off  " Filetype recognition must be disabled for Vundle setup.
set rtp+=$VIMBUNDLE/vundle/
call vundle#begin()

" Vundle plugin manager.
Plugin 'gmarik/vundle'

" Enhanced statusline.
Plugin 'bling/vim-airline'

" Solarized colorscheme.
Plugin 'zoresvit/vim-colors-solarized'

" Intuitive files opening via fuzzy search.
Plugin 'wincent/command-t'

" File browser.
Plugin 'scrooloose/nerdtree'

" File tags browser.
Plugin 'majutsushi/tagbar'

" Ultimate autocompletion.
Plugin 'Valloric/YouCompleteMe'

" Ultimate static syntax analysis.
Plugin 'scrooloose/syntastic'

" Snippets engine.
Plugin 'SirVer/ultisnips'
" Snippets database.
Plugin 'honza/vim-snippets'

" Show git diff in gutter (+/- signs column).
Plugin 'airblade/vim-gitgutter'

" Git repository visualizer (requires `vim-fugitive`).
Plugin 'gregsexton/gitv'
Plugin 'tpope/vim-fugitive'

" Navigating matching tags with `%` for HTML, LaTeX, XML and others.
Plugin 'vim-scripts/matchit.zip'

" Python code editing.
Plugin 'klen/python-mode'

" Activate virtualenvs from Vim.
Plugin 'jmcantrell/vim-virtualenv'

" Puppet manifests editing.
Plugin 'rodjek/vim-puppet'

" DNS Zone files editing.
Plugin 'seveas/bind.vim'

" Syntax highlight for Jinja2 template engine.
Plugin 'Glench/Vim-Jinja2-Syntax'

" Enhanced editing of Tmux configuration file (.tmux.conf).
Plugin 'tmux-plugins/vim-tmux'

" Editing binary files.
Plugin 'fidian/hexmode'

" Enhanced Dockerfile editing.
Plugin 'ekalinin/Dockerfile.vim'

" Syntax highlighting for Conky.
Plugin 'smancill/conky-syntax.vim'

call vundle#end()
filetype plugin indent on

" }}}

" {{{ Configurations

" Airline
" ~~~~~~~

" Unicode symbols.
let g:airline_powerline_fonts = 1
if !exists('g:airline_symbols')
    let g:airline_symbols = {}
endif
let g:airline_left_sep = ''
let g:airline_right_sep = ''
let g:airline_left_alt_sep = ''
let g:airline_right_alt_sep = ''
let g:airline_symbols.linenr = '¶'
let g:airline_symbols.branch = '⎇'
let g:airline_symbols.paste = '∥'
let g:airline_symbols.readonly = 'R'
let g:airline_symbols.whitespace = 'Ξ'

let g:airline#extensions#tabline#tab_nr_type = 2
let g:airline_section_y = airline#section#create_right(['ffenc', '0x%02B'])
let g:airline_section_z = airline#section#create(['windowswap', '%p%% ', 'linenr', ':%-v', ':0x%03O'])

" Solarized
" ~~~~~~~~~

if isdirectory($VIMBUNDLE . "/vim-colors-solarized")
    let g:solarized_bold=0
    let g:solarized_underline=0
    let g:solarized_italic=0
    colorscheme solarized
endif

" Commant-t
" ~~~~~~~~~

let g:CommandTAlwaysShowDotFiles = 1
let g:CommandTCancelMap = ['<ESC>', '<C-c>']
let g:CommandTMaxHeight = 10

" NERDTree
" ~~~~~~~~

imap <F2> :NERDTreeToggle<CR>
nmap <F2> :NERDTreeToggle<CR>

" Tagbar
" ~~~~~~

imap <F3> :TagbarToggle<CR>
nmap <F3> :TagbarToggle<CR>

" YouCompleteMe
" ~~~~~~~~~~~~~

let g:ycm_global_ycm_extra_conf = '/home/zoresvit/.vim/.ycm_extra_conf.py'
let g:ycm_seed_identifiers_with_syntax = 1
nmap <leader>j :YcmCompleter GoTo<CR>

" Syntastic
" ~~~~~~~~~

" HTML5 lint with http://www.htacg.org/tidy-html5.
let g:syntastic_html_tidy_exec = 'tidy5'

" Utlisnips
" ~~~~~~~~~

let g:UltiSnipsExpandTrigger       = '<c-\>'
let g:UltiSnipsListSnippets        = '<c-l>'
let g:UltiSnipsJumpForwardTrigger  = '<c-j>'
let g:UltiSnipsJumpBackwardTrigger = '<c-k>'

let g:UltiSnipsEditSplit="horizontal"

" Python-mode
" ~~~~~~~~~~~

let g:pymode_lint = 1
let g:pymode_lint_on_write = 0
let g:pymode_lint_checkers = ['pep8', 'mccabe']

let g:pymode_rope = 0
let g:pymode_folding = 0
let g:pymode_indent = 1
let g:pymode_motion = 1  " Enable python-specific motions.

let g:pymode_trim_whitespaces = 1
let g:pymode_breakpoint = 1
let g:pymode_breakpoint_bind = '<leader>B'
let g:pymode_syntax = 1
let g:pymode_syntax_all = 1

" }}}
