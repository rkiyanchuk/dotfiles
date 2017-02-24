let $VIMHOME=$HOME . "/.vim"
let $VIMPLUGINS=$VIMHOME . "/plugins"

" Create directories required by Vim configuration.
if !isdirectory($VIMHOME . "/backups")
    call mkdir($VIMHOME . "/backups", "p")
endif
if !isdirectory($VIMHOME . "/swap")
    call mkdir($VIMHOME . "/swap", "p")
endif


" {{{ OPTIONS

syntax on

set completeopt=longest,menuone
"set fillchars=
"set showfulltag
set background=dark
set backspace=indent,eol,start
set backup
set backupdir=$VIMHOME/backups
set browsedir=buffer
set colorcolumn=80
set cursorcolumn
set cursorline
set directory=$VIMHOME/swap
set fileencodings=utf-8,windows-1251,iso-8859015,koi8-r,latin1
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
set showbreak=↪
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
set wildmode=longest,full

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
set shiftround

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


" {{{ FUNCTIONS

if !exists("*ReloadConfig")
    " Reload .vimrc and .gvimrc configuration files.
    function! ReloadConfig()
        source $MYVIMRC
        if has("gui_running")
          source $MYGVIMRC
        endif
    endfunction
endif

function! AdjustWindowHeight(minheight, maxheight)
    " Adjust QuickFix window height according to the number of data lines.
    exe max([min([line("$"), a:maxheight]), a:minheight]) . "wincmd _"
endfunction

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


" {{{ AUTOCOMMANDS

" Close omni-completion preview window when entering or leaving insert mode.
autocmd CursorMovedI * if pumvisible() == 0|pclose|endif
autocmd InsertLeave * if pumvisible() == 0|pclose|endif

augroup TEXT
    " Auto commands for any text file.
    autocmd!
    au FileType text,markdown set spell
    " Enable quickfix window height adjustment.
    au FileType qf call AdjustWindowHeight(3, 6)
augroup END

augroup CPP
    autocmd!
    au FileType c,cpp,h set cindent
    au FileType c,cpp,h set cinoptions = "h3,l1,g1,t0,i4,+4,(0,w1,W4"
augroup END

augroup LATEX
    autocmd!
    au FileType tex set spell
augroup END

augroup WEB
    autocmd!
    au FileType html,yaml,xml set shiftwidth=2
    au FileType html,yaml,xml set softtabstop=2
    au FileType html,yaml,xml set tabstop=2

    au FileType xml setlocal foldmethod=syntax
    au FileType xml normal zR  " Open all folds by default.
augroup END

augroup MISC
    autocmd!
    " Treat Xmobar config as Haskell file.
    au BufRead,BufNewFile .xmobarrc set filetype=haskell
    " Treat .conf files as .cfg.
    au BufRead,BufNewFile *.conf set filetype=cfg
augroup END

" }}}


" {{{ COMMANDS

" Remove trailing spaces from file.
command! -bar -nargs=0 -range=% TrimSpaces <line1>,<line2>call TrimSpaces()

" Save restricted file opened without root permissions via sudo.
command! W :w !sudo tee %

" }}}


" {{{ MAPPINGS

nmap <silent> <leader>V :split $MYVIMRC<CR>
nmap <silent> <leader>R :call ReloadConfig()<CR>
nmap <silent> <leader>s :set spell!<CR>

" }}}


" {{{ PLUGINS

call plug#begin($VIMPLUGINS)

" Essentials
" ==========

Plug 'bling/vim-airline'  " Enhanced status line.
Plug 'vim-airline/vim-airline-themes'
Plug 'zoresvit/vim-colors-solarized'
Plug 'Shougo/unite.vim'  " Fuzzy search for files and buffers.
Plug 'lyokha/vim-xkbswitch'  " Automatic keyboard layout switcher.
Plug 'scrooloose/nerdtree', { 'on': 'NERDTreeToggle' }
Plug 'majutsushi/tagbar', {'on': 'TagbarToggle'}   " File tags browser.
Plug 'SirVer/ultisnips'  " Snippets engine.
Plug 'honza/vim-snippets'  " Snippets database.
Plug 'airblade/vim-gitgutter'  " Show git diff in gutter (+/- signs column).
Plug 'tpope/vim-fugitive'  " Git interface for Vim.
Plug 'gregsexton/gitv'  " Git repository visualizer (requires vim-fugitive).
Plug 'mkitt/tabline.vim'  " Better tabs naming.
Plug 'sjl/gundo.vim'  " Browse Vim undo tree graph.
Plug 'wincent/ferret'  " Multi-file search.
Plug 'Shougo/vimproc'

" Programming
" ===========

Plug 'neomake/neomake' | Plug 'dojoteef/neomake-autolint'
Plug 'davidhalter/jedi-vim', {'for': 'python'}
Plug 'Shougo/neocomplete.vim'
Plug 'lervag/vimtex', { 'for': 'tex' }
Plug 'Glench/Vim-Jinja2-Syntax', { 'for': 'jinja' }
Plug 'rodjek/vim-puppet', { 'for': 'puppet' }
Plug 'artur-shaik/vim-javacomplete2', { 'for': 'java' }
Plug 'jszakmeister/markdown2ctags'
Plug 'jszakmeister/rst2ctags'
Plug 'mbr/vim-pyre'
Plug 'vim-scripts/indentpython.vim'
" Haskell
Plug 'eagletmt/ghcmod-vim', { 'for': 'haskell' }
Plug 'eagletmt/neco-ghc', { 'for': 'haskell' }

" Enhancements
" ============

Plug 'Firef0x/PKGBUILD.vim'
Plug 'seveas/bind.vim'  " Edit DNS Zone files.
Plug 'tmux-plugins/vim-tmux'  " Edit Tmux configuration file.
Plug 'fidian/hexmode'  " Edit binary files.
Plug 'ekalinin/Dockerfile.vim'  " Edit Dockerfile.
Plug 'smancill/conky-syntax.vim'  " Syntax highlighting for Conky.
Plug 'gabrielelana/vim-markdown'  " Edit Markdown.
Plug 'hrother/msmtp.vim'  " msmtprc syntax highlighting.
Plug 'hrother/offlineimaprc.vim'  " offlineimaprc highlighting.
Plug 'Matt-Deacalion/vim-systemd-syntax'
Plug 'chase/vim-ansible-yaml'  " Ansible syntax highlighting and snippets.
Plug 'rhysd/vim-grammarous'

call plug#end()

" }}}

" {{{ PLUGINS CONFIGURATION

" Airline
" -------

let g:airline_powerline_fonts = 1

if !exists('g:airline_symbols')
    let g:airline_symbols = {}
endif
let g:airline_left_sep = ''
let g:airline_right_sep = ''
let g:airline_left_alt_sep = ''
let g:airline_right_alt_sep = ''
let g:airline_symbols.linenr = '¶'
let g:airline_symbols.paste = '▼'
let g:airline_symbols.readonly = ''
let g:airline_symbols.whitespace = '∅'
let g:airline#extensions#tabline#tab_nr_type = 2
let g:airline#extensions#tagbar#enabled = 0
let g:airline_section_y = airline#section#create_right(['ffenc', '0x%02B'])
let g:airline_section_z = airline#section#create(['windowswap', '%p%% ', 'linenr', ':%-v', ':0x%03O'])


" xkbswitch
" ---------

let g:XkbSwitchEnabled = 1
let g:XkbSwitchSkipFt = [ 'nerdtree' ]

" Solarized
" ---------

if isdirectory($VIMPLUGINS . "/vim-colors-solarized")
    let g:solarized_bold=0
    let g:solarized_underline=0
    let g:solarized_italic=0
    colorscheme solarized
endif

" Unite
" -----

call unite#custom#profile('default', 'context', {'winheight': 10})
nnoremap <leader>f :Unite -no-split -start-insert file_rec<CR>
nnoremap <leader>b :Unite -no-split buffer<CR>
let g:unite_enable_auto_select=0

" NERDTree
" --------

imap <leader>1 :NERDTreeToggle<CR>
nmap <leader>1 :NERDTreeToggle<CR>
let NERDTreeIgnore = ['\.pyc$']

" Tagbar
" ------

imap <leader>2 :TagbarToggle<CR>
nmap <leader>2 :TagbarToggle<CR>

let g:tagbar_type_css = {
\ 'ctagstype' : 'Css',
    \ 'kinds'     : [
        \ 'c:classes',
        \ 's:selectors',
        \ 'i:identities'
    \ ]
\ }

let g:tagbar_type_make = {
            \ 'kinds':[
                \ 'm:macros',
                \ 't:targets'
            \ ]
\}

let g:tagbar_type_markdown = {
    \ 'ctagstype': 'markdown',
    \ 'ctagsbin' : '$VIMPLUGINS/markdown2ctags/markdown2ctags.py',
    \ 'ctagsargs' : '-f - --sort=yes',
    \ 'kinds' : [
        \ 's:sections',
        \ 'i:images'
    \ ],
    \ 'sro' : '|',
    \ 'kind2scope' : {
        \ 's' : 'section',
    \ },
    \ 'sort': 0,
\ }

let g:tagbar_type_puppet = {
    \ 'ctagstype': 'puppet',
    \ 'kinds': [
        \'c:class',
        \'s:site',
        \'n:node',
        \'d:definition'
      \]
    \}

let g:tagbar_type_rst = {
    \ 'ctagstype': 'rst',
    \ 'ctagsbin' : '$VIMPLUGINS/rst2ctags/rst2ctags.py',
    \ 'ctagsargs' : '-f - --sort=yes',
    \ 'kinds' : [
        \ 's:sections',
        \ 'i:images'
    \ ],
    \ 'sro' : '|',
    \ 'kind2scope' : {
        \ 's' : 'section',
    \ },
    \ 'sort': 0,
\ }



" UtliSnips
" ---------

let g:UltiSnipsExpandTrigger       = '<c-\>'
let g:UltiSnipsListSnippets        = '<c-l>'
let g:UltiSnipsJumpForwardTrigger  = '<c-j>'
let g:UltiSnipsJumpBackwardTrigger = '<c-k>'
let g:UltiSnipsEditSplit = "horizontal"
let g:ultisnips_python_style = "sphinx"

" vimtex
" ------

let g:vimtex_latexmk_enabled = 0
let g:vimtex_fold_enabled = 0

" jedi-vim
autocmd FileType python setlocal omnifunc=jedi#completions
let g:jedi#completions_enabled = 0
let g:jedi#auto_vim_configuration = 0

" Neocomplete
let g:neocomplete#enable_at_startup = 1
if !exists('g:neocomplete#force_omni_input_patterns')
        let g:neocomplete#force_omni_input_patterns = {}
endif
let g:neocomplete#force_omni_input_patterns.python = '\%([^. \t]\.\|^\s*@\|^\s*from\s.\+import \|^\s*from \|^\s*import \)\w*'
