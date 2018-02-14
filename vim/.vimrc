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
set showfulltag
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
set listchars=tab:→\ ,space:·,extends:▶,precedes:◀,nbsp:␣
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
" Set native status line as fallback.
set statusline=%f\ %m\ %r\ %y\ [%{&fileencoding}]\ [len\ %L:%p%%]
set statusline+=\ [pos\ %02l:%02c\ 0x%O]\ [chr\ %3b\ 0x%02B]\ [buf\ #%n]
set termguicolors
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
set incsearch
set nowrapscan
set ignorecase
set infercase
set smartcase

" Indent
" ------

set breakindent
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

function! FixSpaces() range
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
    au FileType gitcommit set colorcolumn=73
    au FileType gitcommit set textwidth=72
    " Treat .conf files as .cfg.
    au BufRead,BufNewFile *.conf set filetype=cfg
augroup END

" }}}


" {{{ COMMANDS

" Remove trailing spaces from file.
command! -bar -nargs=0 -range=% FixSpaces <line1>,<line2>call FixSpaces()

" Save restricted file opened without root permissions via sudo.
command! W :w !sudo tee %

command! Update :call ReloadConfig() | PlugUpdate | PlugUpgrade

" }}}


" {{{ MAPPINGS

nmap <silent> <leader>V :split $MYVIMRC<CR>
nmap <silent> <leader>R :call ReloadConfig()<CR>
nmap <silent> <leader>s :set spell!<CR>

" Reset search highlighting by pressing Enter in normal mode.
nnoremap <Esc><Esc> :noh<CR>

" }}}


" {{{ PLUGINS

" Auto install vim-plug plugin manager.
if empty(glob($VIMHOME . '/autoload/plug.vim'))
  silent !curl -fLo ~/.vim/autoload/plug.vim --create-dirs
    \ https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
  autocmd VimEnter * PlugInstall --sync | source $MYVIMRC
endif

call plug#begin($VIMPLUGINS)

Plug 'icymind/NeoSolarized'  " Solarized colorscheme with true color support.
Plug 'Shougo/vimproc' " Asynchronous execution library for Vim.
Plug 'Shougo/denite.nvim'  " Fuzzy search for files and buffers.
Plug 'sjl/gundo.vim'  " Browse Vim undo tree graph.
Plug 'lyokha/vim-xkbswitch'  " Automatic keyboard layout switcher.
Plug 'scrooloose/nerdtree', { 'on': 'NERDTreeToggle' }
Plug 'majutsushi/tagbar'  " File tags browser.
Plug 'mkitt/tabline.vim'  " Better tabs naming.


" Snippets
Plug 'SirVer/ultisnips'  " Snippets engine.
Plug 'honza/vim-snippets'  " Snippets database.

" Git
Plug 'airblade/vim-gitgutter'  " Show git diff in gutter (+/- signs column).
Plug 'gregsexton/gitv' | Plug 'tpope/vim-fugitive'  " Git interface for Vim.

" Statusline
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'

" Programming
" ===========

" Completion
Plug 'Shougo/deoplete.nvim'
Plug 'roxma/nvim-yarp'
Plug 'roxma/vim-hug-neovim-rpc'
Plug 'zchee/deoplete-clang'
Plug 'zchee/deoplete-go', {'for': 'python', 'do': 'make'}
Plug 'zchee/deoplete-jedi', {'for': 'python'}
Plug 'davidhalter/jedi-vim', {'for': 'python'}
Plug 'artur-shaik/vim-javacomplete2', { 'for': 'java' }

" Static analysis and formatting.
Plug 'neomake/neomake' | Plug 'dojoteef/neomake-autolint'

Plug 'jiangmiao/auto-pairs'
Plug 'fidian/hexmode'  " Edit binary files in hex.
"Plug 'fatih/vim-go', { 'for': 'go' }
Plug 'lervag/vimtex', { 'for': 'tex' }

" Spell check
Plug 'rhysd/vim-grammarous'

" Filetypes and syntax
Plug 'Glench/Vim-Jinja2-Syntax', { 'for': 'jinja' }
Plug 'rodjek/vim-puppet', { 'for': 'puppet' }
Plug 'jszakmeister/markdown2ctags'
Plug 'jszakmeister/rst2ctags'
Plug 'seveas/bind.vim'  " Edit DNS Zone files.
Plug 'tmux-plugins/vim-tmux'  " Edit Tmux configuration file.
Plug 'ekalinin/Dockerfile.vim'  " Edit Dockerfile.
Plug 'smancill/conky-syntax.vim'  " Syntax highlighting for Conky.
Plug 'gabrielelana/vim-markdown'  " Edit Markdown.
Plug 'hrother/msmtp.vim'  " msmtprc syntax highlighting.
Plug 'hrother/offlineimaprc.vim'  " offlineimaprc highlighting.
Plug 'Matt-Deacalion/vim-systemd-syntax'
Plug 'chase/vim-ansible-yaml'  " Ansible syntax highlighting and snippets.
Plug 'hashivim/vim-vagrant'
Plug 'hashivim/vim-terraform'

call plug#end()

let g:deoplete#enable_at_startup = 1

" }}}

" {{{ PLUGINS CONFIGURATION

" vim-airline
" -----------

let g:airline_powerline_fonts = 1
let g:airline_left_sep = ''
let g:airline_right_sep = ''
let g:airline_left_alt_sep = ''
let g:airline_right_alt_sep = ''
let g:airline_skip_empty_sections = 1

" vim-xkbswitch
" -------------

let g:XkbSwitchEnabled = 1
let g:XkbSwitchSkipFt = [ 'nerdtree' ]

" NeoSolarized
" ------------

if isdirectory($VIMPLUGINS . "/NeoSolarized")
    colorscheme NeoSolarized
endif

" gitgutter
" ---------
let g:gitgutter_override_sign_column_highlight = 0

" Denite
" ------
if isdirectory($VIMPLUGINS . '/denite.nvim')
    nnoremap <leader>ff :Denite file_rec<CR>
    nnoremap <leader>fb :Denite buffer<CR>
    nnoremap <leader>fg :Denite grep<CR>
    nnoremap <leader>fr :Denite register<CR>
    nnoremap <leader>fw :DeniteCursorWord file_rec buffer grep<CR>

    call denite#custom#var('file_rec', 'command',
                \ ['rg', '--files', '--glob', '!.git', ''])

    call denite#custom#var('grep', 'command', ['rg'])
    call denite#custom#var('grep', 'default_opts', ['--vimgrep', '--no-heading'])
    call denite#custom#var('grep', 'recursive_opts', [])
    call denite#custom#var('grep', 'pattern_opt', ['--regexp'])
    call denite#custom#var('grep', 'separator', ['--'])
    call denite#custom#var('grep', 'final_opts', [])

    call denite#custom#map('insert', '<C-n>', '<denite:move_to_next_line>', 'noremap')
    call denite#custom#map('insert', '<C-p>', '<denite:move_to_previous_line>', 'noremap')
    call denite#custom#map('insert', '<C-j>', '<denite:assign_next_text>', 'noremap')
    call denite#custom#map('insert', '<C-k>', '<denite:assign_previous_text>', 'noremap')
else
    echomsg 'denite.vim is not installed.'
endif

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

autocmd FileType java setlocal omnifunc=javacomplete#Complete

" Neomake
let g:neomake_autolint_sign_column_always = 1

" Auto-Pairs
let g:AutoPairsShortcutFastWrap = '<leader>w'
let g:AutoPairsShortcutJump = '<C-l>'

let g:gundo_prefer_python3 = 1

" jedi-vim
let g:jedi#completions_enabled = 0
let g:jedi#use_tabs_not_buffers = 1

" }}}
