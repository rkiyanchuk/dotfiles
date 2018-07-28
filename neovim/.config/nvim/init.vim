let $VIMHOME=$HOME . "/.config/nvim"
let $VIMSITE=$HOME . "/.local/share/nvim/site"
let $VIMPLUGINS=$VIMSITE . "/plugins"

" Create directories required by Vim configuration.
if !isdirectory($VIMSITE . "/backups")
    call mkdir($VIMSITE . "/backups", "p")
endif
if !isdirectory($VIMSITE . "/swap")
    call mkdir($VIMSITE . "/swap", "p")
endif
if !isdirectory($VIMSITE . "/undo")
    call mkdir($VIMSITE . "/undo", "p")
endif

set background=dark
set backup
set backupdir=$VIMSITE/backups
set backspace=indent,eol,start
set colorcolumn=80
set cursorcolumn
set cursorline
set directory=$VIMSITE/swap  " Store swap files here instead of current dir.
set formatoptions+=r  " Automatically insert current comment leader on Enter.
set hidden  " Hide current buffer when opening new file instead of closing it.
set listchars=tab:→\ ,space:·,extends:▶,precedes:◀,nbsp:␣
set matchpairs+=<:>
set mouse=a
set mousemodel=popup_setpos
set nowrap
set number
set path+=**  " Search downwards in a directory for `gf`, etc.
set scrolloff=3
set showbreak=↪
set showfulltag  " Show arguments for a function when available, etc.
set spelllang=en_us,ru_yo,uk
set splitbelow
set splitright
set termguicolors
set textwidth=79
set undodir=$VIMSITE/undo
set undofile
set updatetime=1000  " For more efficient Tagbar functioning
set virtualedit=all
set wildmenu

" Search
set hlsearch
set incsearch
set nowrapscan
set ignorecase
set infercase
set smartcase

" Indent
set breakindent
set autoindent
set expandtab
set shiftwidth=4
set softtabstop=4
set tabstop=4
set shiftround

" Set native status line as fallback.
set laststatus=2
set statusline=%f\ %m\ %r\ %y\ [%{&fileencoding}]\ [len\ %L:%p%%]
set statusline+=\ [pos\ %02l:%02c\ 0x%O]\ [chr\ %3b\ 0x%02B]\ [buf\ #%n]

" Filetype settings
let c_comment_strings = 1
let c_space_errors = 1  " Highlight extra white spaces.
let g:load_doxygen_syntax = 1
let g:tex_flavor = "latex"  " Consider .tex files as LaTeX instead of plainTeX.
let g:xml_syntax_folding = 1

" Prevent Neovim from calling Python on startup.
" See https://github.com/neovim/neovim/issues/5728#issuecomment-265454125
let g:python3_host_skip_check = 0
let g:python_host_skip_check = 0


" MAPPINGS
" ========

nnoremap <silent> <leader>V :split $MYVIMRC<CR>
nnoremap <silent> <leader>R :call ReloadConfig()<CR>
nnoremap <silent> <leader>co :copen<CR>
nnoremap <silent> <leader>cc :cclose<CR>

" Reset search highlighting by double pressing Esc in normal mode.
nnoremap <Esc><Esc> :nohlsearch<CR>


" FUNCTIONS
" =========

if !exists("*ReloadConfig")
    " Reload .vimrc and .gvimrc configuration files.
    function! ReloadConfig()
        source $MYVIMRC
        if has("gui_running")
          source $MYGVIMRC
        endif
        redraw | echomsg "Sourced Vim config!"
        " Add second line to make Vim prompt for continuation.
    endfunction
endif

function! ShowSpaces(...)
    " Highlight trailing spaces.
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
    " Remove trailing spaces.
    let oldhlsearch=ShowSpaces(1)
    execute a:firstline.",".a:lastline."substitute ///ge"
    let &hlsearch=oldhlsearch
endfunction


" COMMANDS
" ========

command! UltiSnipsListSnippets :call UltiSnips#ListSnippets()

" Remove trailing spaces from file.
command! -bar -nargs=0 -range=% FixSpaces <line1>,<line2>call FixSpaces()

" Update plugins.
command! Update :call ReloadConfig() | PlugUpdate | PlugUpgrade


" AUTOCOMMANDS
" ============

augroup TEXT
    " Auto commands for any text files.
    autocmd!
    au FileType text,markdown,tex set spell
augroup END

augroup CPP
    autocmd!
    au FileType c,cpp,h set cindent
    au FileType c,cpp,h set cinoptions = "h3,l1,g1,t0,i4,+4,(0,w1,W4"
augroup END

augroup HTML
    autocmd!
    au FileType html setlocal shiftwidth=2
    au FileType html setlocal softtabstop=2
    au FileType html setlocal tabstop=2
augroup END

augroup MISC
    autocmd!
    au FileType gitcommit set colorcolumn=73
    au FileType gitcommit set textwidth=72

    au BufRead,BufNewFile *.conf set filetype=cfg  " Treat .conf files as .cfg.

    " Open all folds by default
    au Syntax * normal zR

    " Auto close omni-completion preview window.
    au CursorMovedI * if pumvisible() == 0|pclose|endif
    au CompleteDone * if pumvisible() == 0|pclose|endif

    " Make <K> to open ansible-doc when editing playbooks.
    au FileType ansible set keywordprg=ansible-doc

augroup END


" PLUGINS
" =======

" Auto install vim-plug plugin manager.
if empty(glob($VIMSITE . '/autoload/plug.vim'))
  silent !curl -fLo ~/.local/share/nvim/site/autoload/plug.vim --create-dirs
    \ https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
  autocmd VimEnter * PlugInstall --sync | source $MYVIMRC
endif


call plug#begin($VIMPLUGINS)

" Essentials
" ==========

Plug 'junegunn/vim-plug'  " Generate :help for vim-plug itself.
Plug 'icymind/NeoSolarized'
Plug 'vim-airline/vim-airline' | Plug 'vim-airline/vim-airline-themes'

Plug 'scrooloose/nerdtree', {'on': 'NERDTreeToggle'}
Plug 'majutsushi/tagbar', {'on': 'TagbarToggle'}
    Plug 'jszakmeister/markdown2ctags'
    Plug 'jszakmeister/rst2ctags'

Plug 'sjl/gundo.vim', {'on': 'GundoToggle'}  " Browse change history tree.
Plug 'Shougo/denite.nvim'  " Fuzzy search for files and buffers.
Plug 'SirVer/ultisnips' | Plug 'honza/vim-snippets'
Plug 'jiangmiao/auto-pairs'  " Auto-matching braces.
Plug 'neomake/neomake'  " Static analysis and formatting.
Plug 'sheerun/vim-polyglot'  " Syntax and indent pack for many languages.

Plug 'airblade/vim-gitgutter'  " Show git diff in gutter (+/- signs column).
Plug 'gregsexton/gitv', {'on': ['Gitv']} | Plug 'tpope/vim-fugitive'

" Completion
" ----------

Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' }

Plug 'zchee/deoplete-jedi', {'for': 'python'}
Plug 'davidhalter/jedi-vim', {'for': 'python'}
Plug 'python-mode/python-mode', {'for': 'python'}
Plug 'artur-shaik/vim-javacomplete2', {'for': 'java'}
Plug 'fatih/vim-go', {'for': 'go', 'do': ':GoInstallBinaries'}

Plug 'Shougo/neoinclude.vim', {'for': ['c', 'cpp', 'cxx']}
if executable("clang")
    Plug 'zchee/deoplete-clang', {'for': ['c', 'cpp', 'cxx']}
else
    echomsg "Install clang package for C/C++ completion support!"
endif

if executable("gocode")
    Plug 'zchee/deoplete-go', {'for': 'go', 'do': 'make'}
else
    echomsg "Install gocode package for Go lang completion support!"
endif

" Enhancements for specific file types.
Plug 'tmux-plugins/vim-tmux'
Plug 'ekalinin/Dockerfile.vim'
Plug 'smancill/conky-syntax.vim'
Plug 'Matt-Deacalion/vim-systemd-syntax'
Plug 'gabrielelana/vim-markdown'
Plug 'pearofducks/ansible-vim'
Plug 'hashivim/vim-vagrant'
Plug 'hashivim/vim-terraform'
Plug 'fidian/hexmode'

call plug#end()

" NeoSolarized
" ------------

" Silent suppresses errors when colorscheme plugin is not yet installed.
silent! colorscheme NeoSolarized
let g:neosolarized_bold = 0

" nerdtree
" --------

inoremap <leader>1 :NERDTreeToggle<CR>
nnoremap <leader>1 :NERDTreeToggle<CR>
let NERDTreeIgnore = ['\.pyc$']

" Close Vim if NERDTree is the only window left.
autocmd BufEnter * if (winnr("$") == 1 && exists("b:NERDTree") && b:NERDTree.isTabTree()) | q | endif

" tagbar
" ------

inoremap <leader>2 :TagbarToggle<CR>
nnoremap <leader>2 :TagbarToggle<CR>
source $VIMHOME/tagbar_types.vim

" gundo
" -----

inoremap <leader>3 :GundoToggle<CR>
nnoremap <leader>3 :GundoToggle<CR>
let g:gundo_prefer_python3 = 1

" denite
" ------

nnoremap <leader>ff :Denite file_rec<CR>
nnoremap <leader>fb :Denite buffer<CR>
nnoremap <leader>fg :Denite grep<CR>
nnoremap <leader>fr :Denite register<CR>
nnoremap <leader>fw :DeniteCursorWord file_rec buffer grep<CR>
if exists('g:loaded_denite')
    call denite#custom#map('insert', '<C-n>', '<denite:move_to_next_line>', 'noremap')
    call denite#custom#map('insert', '<C-p>', '<denite:move_to_previous_line>', 'noremap')
    call denite#custom#map('insert', '<C-j>', '<denite:assign_next_text>', 'noremap')
    call denite#custom#map('insert', '<C-k>', '<denite:assign_previous_text>', 'noremap')

    call denite#custom#var('grep', 'command', ['rg'])
    call denite#custom#var('grep', 'default_opts',
            \ ['--vimgrep', '--no-heading'])
    call denite#custom#var('grep', 'recursive_opts', [])
    call denite#custom#var('grep', 'pattern_opt', ['--regexp'])
    call denite#custom#var('grep', 'separator', ['--'])
    call denite#custom#var('grep', 'final_opts', [])
endif


" vim-airline
" -----------

let g:airline_powerline_fonts = 1
let g:airline_left_sep = ''
let g:airline_right_sep = ''
let g:airline_left_alt_sep = ''
let g:airline_right_alt_sep = ''
let g:airline_skip_empty_sections = 1

if exists('g:loaded_airline')
    call airline#parts#define_raw('char', '§ %2Bh')
    let g:airline_section_y = airline#section#create_left(['char', 'ffenc'])
    let g:airline#extensions#tabline#enabled = 1
    let g:airline#extensions#tabline#show_buffers = 0
    let g:airline#extensions#tabline#show_splits = 0
    let g:airline#extensions#tabline#show_tab_type = 0
    let g:airline#extensions#tabline#show_tabs = 1
    let g:airline#extensions#tabline#tab_min_count = 2
    let g:airline#extensions#tabline#tab_nr_type = 1  " Show tab number
endif

" gitgutter
" ---------

let g:gitgutter_override_sign_column_highlight = 0

" deoplete
" --------

autocmd InsertEnter * call deoplete#enable()  " Enable Deoplete on insert.
let g:deoplete#sources#clang#libclang_path="/usr/lib/libclang.so"
let g:deoplete#sources#clang#clang_header="/usr/include/clang"

" deoplete-jedi
" -------------

" Force Jedi to use system python when working from virtualenv.
if has('mac')
    let g:python_host_prog = '/usr/local/bin/python2'
    let g:python3_host_prog = '/usr/local/bin/python3'
elseif has('unix')
    let g:python_host_prog = '/usr/bin/python2'
    let g:python3_host_prog = '/usr/bin/python3'
endif

" UtliSnips
" ---------

let g:ultisnips_python_style = "sphinx"

" Neomake
" -------

let g:neomake_autolint_sign_column_always = 1
let g:neomake_open_list = 2
" Enable automake if Neomake plugin is loaded.
" exists() doesn't work because plugins loaded after .vimrc is read.
autocmd BufReadPost * if exists(":Neomake") | exe "call neomake#configure#automake('irw', 1000)" | endif

" python-mode
" -----------

let g:pymode_python = 'python3'
let g:pymode_doc = 1
let g:pymode_indent = 1
let g:pymode_folding = 0
let g:pymode_lint = 0
let g:pymode_virtualenv = 0
let g:pymode_rope_completion = 0
let g:pymode_trim_whitespaces = 0
let g:pymode_debug = 0
let g:pymode_rope = 0

" auto-pairs
" ----------

let g:AutoPairsShortcutJump = '<C-l>'

" vim-javacomplete2
" -----------------

let g:JavaComplete_UsePython3 = 1

" vim-go
" ------

nnoremap <leader>g :GoDef<CR>
let g:go_highlight_types = 1
let g:go_highlight_fields = 1
let g:go_highlight_functions = 1
let g:go_highlight_methods = 1
let g:go_highlight_extra_types = 1


" UltiSnips
" ---------

let g:UltiSnipsExpandTrigger = '<C-\>'