let $VIMHOME=fnamemodify($MYVIMRC, ':h')
let $VIMSITE=$HOME . "/.local/share/nvim/site"
let $VIMPLUGINS=$VIMSITE . "/plugins"


" PLUGINS
" =======

" Auto install vim-plug plugin manager.
if !filereadable($VIMHOME . '/autoload/plug.vim')
    if executable('curl')
        silent !curl -fLo $VIMHOME/autoload/plug.vim --create-dirs
                    \ https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
        autocmd VimEnter * PlugInstall --sync | source $MYVIMRC
    else
        echomsg "Install curl to download vim-plug plugin manager!"
    endif
endif

call plug#begin($VIMPLUGINS)

Plug 'junegunn/vim-plug'  " Generate :help for vim-plug itself.
Plug 'icymind/NeoSolarized'  " Solarized colorscheme.
Plug 'vim-airline/vim-airline' | Plug 'vim-airline/vim-airline-themes'
Plug 'scrooloose/nerdtree', {'on': 'NERDTreeToggle'}
Plug 'sjl/gundo.vim', {'on': 'GundoToggle'}  " Browse change history tree.
Plug 'majutsushi/tagbar', {'on': 'TagbarToggle'}
    Plug 'jszakmeister/markdown2ctags'
    Plug 'jszakmeister/rst2ctags'

Plug 'Shougo/denite.nvim'  " Fuzzy search for files, buffers and other sources.
Plug 'SirVer/ultisnips' | Plug 'honza/vim-snippets'

Plug 'gregsexton/gitv', {'on': ['Gitv']}
    Plug 'tpope/vim-fugitive'
Plug 'mhinz/vim-signify'  " Show file diff in signcolumn.

" Use my fork of auto-pairs until upstream merges the pull request:
" https://github.com/jiangmiao/auto-pairs/pull/222
Plug 'zoresvit/auto-pairs', {'branch': 'patch-2'}
"Plug 'jiangmiao/auto-pairs'

" Completion and programming.
Plug 'Shougo/deoplete.nvim', {'do': ':UpdateRemotePlugins'}
Plug 'autozimu/LanguageClient-neovim', {'branch': 'next', 'do': 'bash install.sh'}
Plug 'Shougo/echodoc.vim'

" Enhancements for specific file types.
Plug 'sheerun/vim-polyglot'  " Syntax and indent pack for many languages.
Plug 'smancill/conky-syntax.vim'  " Syntax for .conkyrc.
Plug 'hashivim/vim-vagrant'
Plug 'fidian/hexmode'  " Hex editor mode.
Plug 'pearofducks/ansible-vim'

call plug#end()


" SETTINGS
" ========

set autoread
set background=dark
set backspace=indent,eol,start
set backup
set clipboard=unnamed,unnamedplus
set colorcolumn=80
set completeopt=longest,menuone
"set completeopt=longest,menuone,preview  " Disable until preview with splitbelow is fixed.
set cursorcolumn
set cursorline
set formatoptions+=r  " Auto-insert current comment leader on Enter.
set hidden  " Hide current buffer when opening new file instead of closing it.
set listchars=tab:→\ ,space:·,extends:▶,precedes:◀,nbsp:␣
set matchpairs+=<:>
set mouse=a
set mousemodel=popup_setpos
set noshowmode  " Mode is already displayed by vim-airline plugin.
set nowrap
set number
set path+=**  " Search downwards in a directory for `gf`, etc.
set scrolloff=3
set showbreak=↪
set showcmd
set showfulltag  " Show arguments for a function when available, etc.
set sidescrolloff=3
set signcolumn=yes
set spelllang=en_us,ru_yo,uk
set splitbelow
set splitright
set termguicolors
set textwidth=79
set title
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

" Directories for temp files.
set undodir=$VIMSITE/undo
set backupdir=$VIMSITE/backups
set directory=$VIMSITE/swap  " Directory to store swap files.

" Ensure directories for Vim temp files exist.
for path in [&undodir, &backupdir, &directory]
    if !isdirectory(expand(path))
        call mkdir(expand(path), "p")
    endif
endfor

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

if has("mac")
    let g:python_host_prog  = '/usr/local/bin/python'
    let g:python3_host_prog = '/usr/local/bin/python3'
else
    let g:python_host_prog  = '/usr/bin/python'
    let g:python3_host_prog = '/usr/bin/python3'
endif

" ==> icymind/NeoSolarized

let g:neosolarized_bold = 0

try
    colorscheme NeoSolarized
catch
    echomsg "NeoSolarized plugin is missing! Using built-in colorscheme."
    colorscheme desert
endtry


" MAPPINGS
" ========

nnoremap <silent> <leader>V :edit $MYVIMRC<CR>
nnoremap <leader>R :call ReloadConfig()<CR>
nnoremap <leader>s :set spell!<CR>

" Reset search highlighting by double pressing Esc in normal mode.
nnoremap <Esc><Esc> :nohlsearch<CR>

inoremap <silent> <leader>1 :NERDTreeToggle<CR>
nnoremap <silent> <leader>1 :NERDTreeToggle<CR>

inoremap <silent> <leader>2 :TagbarToggle<CR>
nnoremap <silent> <leader>2 :TagbarToggle<CR>

inoremap <silent> <leader>3 :GundoToggle<CR>
nnoremap <silent> <leader>3 :GundoToggle<CR>

nnoremap <silent> <leader>ff :Denite file_rec<CR>
nnoremap <silent> <leader>fb :Denite buffer<CR>
nnoremap <silent> <leader>fg :Denite grep<CR>
nnoremap <silent> <leader>fr :Denite register<CR>
nnoremap <silent> <leader>fw :DeniteCursorWord file_rec buffer grep<CR>

function! SetLSPShortcuts()
    nnoremap <silent> <F2> :Denite contextMenu<CR>
    nnoremap <silent> K :call LanguageClient#textDocument_hover()<CR>
    nnoremap <silent> <leader>gd :call LanguageClient#textDocument_definition()<CR>
    nnoremap <silent> <leader>gi :call LanguageClient#textDocument_implementation()<CR>
endfunction()


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

function! FixSpaces()
    " Remove trailing spaces.
    let oldhlsearch=ShowSpaces(1)
    execute a:firstline.",".a:lastline."substitute ///ge"
    let &hlsearch=oldhlsearch
endfunction

function! PluginInstalled(name)
    " Check if plugin has been installed.
    return isdirectory($VIMPLUGINS . "/" . a:name)
endfunction


" COMMANDS
" ========

command! UltiSnipsListSnippets :call UltiSnips#ListSnippets()

" Remove trailing spaces from file.
command! -bar -nargs=0 -range=% FixSpaces <line1>,<line2>call FixSpaces()

" Update plugins.
command! Update :call ReloadConfig() | PlugUpdate | PlugUpgrade | source $MYVIMRC


" AUTOCOMMANDS
" ============

augroup TEXT
    " Auto commands for any text files.
    autocmd!
    au FileType text,markdown,tex setlocal spell
augroup END

augroup CPP
    autocmd!
    au FileType c,cpp,h setlocal cindent
    au FileType c,cpp,h setlocal cinoptions = "h3,l1,g1,t0,i4,+4,(0,w1,W4"
augroup END

augroup HTML
    autocmd!
    au FileType html setlocal shiftwidth=2
    au FileType html setlocal softtabstop=2
    au FileType html setlocal tabstop=2
augroup END

augroup MISC
    autocmd!
    au FileType gitcommit setlocal colorcolumn=73
    au FileType gitcommit setlocal textwidth=72

    au BufRead,BufNewFile *.conf setlocal filetype=cfg  " Treat .conf files as .cfg.

    " Open all folds by default
    au Syntax * normal zR

    " Auto close omni-completion preview window.
    au CursorMovedI,CompleteDone * if pumvisible() == 0|pclose|endif

    " Make <K> to open ansible-doc when editing playbooks.
    au FileType ansible setlocal keywordprg=ansible-doc
augroup END

augroup LSP
  autocmd!
  autocmd FileType c,cpp,objc,objcpp,python,go,sh,java call SetLSPShortcuts()
augroup END


" PLUGIN SETTINGS
" ===============

" ==> scrooloose/nerdtree

let NERDTreeIgnore = ['\.pyc$', '__pycache__']

" Close Vim if NERDTree is the only window left.
autocmd BufEnter * if (winnr("$") == 1 && exists("b:NERDTree") && b:NERDTree.isTabTree()) | q | endif

" ==> majutsushi/tagbar

source $VIMHOME/tagbar_types.vim

" ==> sjl/gundo.vim

let g:gundo_prefer_python3 = 1

" ==> Shougo/denite.nvim

if PluginInstalled('denite.nvim')
    call denite#custom#map('insert', '<C-n>', '<denite:move_to_next_line>', 'noremap')
    call denite#custom#map('insert', '<C-p>', '<denite:move_to_previous_line>', 'noremap')
    call denite#custom#map('insert', '<C-j>', '<denite:assign_next_text>', 'noremap')
    call denite#custom#map('insert', '<C-k>', '<denite:assign_previous_text>', 'noremap')

    call denite#custom#var('file_rec', 'command',
        \ ['rg', '--files', '-g', '!.tox', '-g', '!.tox'])

    call denite#custom#var('grep', 'command', ['rg'])
    call denite#custom#var('grep', 'default_opts', ['--vimgrep', '-uu', '--smart-case', '--no-heading', '-g', '!.tox', '-g', '!.git'])
    call denite#custom#var('grep', 'recursive_opts', [])
    call denite#custom#var('grep', 'pattern_opt', ['--regexp'])
    call denite#custom#var('grep', 'separator', ['--'])
    call denite#custom#var('grep', 'final_opts', [])
endif

" ==> vim-airline/vim-airline

let g:airline_powerline_fonts = 1
let g:airline_skip_empty_sections = 1

" Remove separators (they are distracting).
let g:airline_left_sep = ''
let g:airline_right_sep = ''
let g:airline_left_alt_sep = ''
let g:airline_right_alt_sep = ''

if PluginInstalled("vim-airline")
    " Show character code under cursor.
    call airline#parts#define_raw('char', '§ %2Bh')
    let g:airline_section_y = airline#section#create_left(['char', 'ffenc'])
endif

let g:airline#extensions#tabline#enabled = 1
let g:airline#extensions#tabline#show_buffers = 0
let g:airline#extensions#tabline#show_splits = 0
let g:airline#extensions#tabline#show_tab_type = 0
let g:airline#extensions#tabline#show_tabs = 1
let g:airline#extensions#tabline#tab_min_count = 2
let g:airline#extensions#tabline#tab_nr_type = 1  " Show tab number

" ==> Shougo/deoplete.nvim

if PluginInstalled("deoplete.nvim")
    autocmd InsertEnter * call deoplete#enable()  " Enable Deoplete on insert.
endif

let g:deoplete#sources#clang#libclang_path="/usr/lib/libclang.so"
let g:deoplete#sources#clang#clang_header="/usr/include/clang"

" ==> SirVer/ultisnips

let g:UltiSnipsExpandTrigger = '<C-\>'
let g:ultisnips_python_style = "sphinx"

" ==> mhinz/vim-signify

let g:signify_vcs_list = ['git', 'hg']
let g:signify_sign_delete            = '−'
let g:signify_sign_delete_first_line = '‾'
let g:signify_sign_change            = '~'
let g:signify_sign_changedelete      = '≂'
let g:signify_sign_show_count = 0

" ==> Shougo/echodoc.vim

let g:echodoc#enable_at_startup = 1

" ==> autozimu/LanguageClient-neovim

let g:LanguageClient_settingsPath = $VIMHOME . "/settings.json"
let g:LanguageClient_serverCommands = {}

if executable('pyls')
    let g:LanguageClient_serverCommands.python = ['pyls']
else
    echomsg "Python language server is missing!"
    " Dependencies:
    " - python-jedi
    " - python-rope
    " - flake8
    " - python-pyflakes
    " - python-mccabe
    " - python-pycodestyle
    " - python-pydocstyle
    " - yapf
endif

if executable('bash-language-server')
    let g:LanguageClient_serverCommands.sh = ['bash-language-server', 'start']
else
    echomsg "Bash language server is missing!"
    " Dependencies:
    " - bash-language-server
endif

if executable('clangd')
    let g:LanguageClient_serverCommands.c = ['clangd']
    let g:LanguageClient_serverCommands.objc = ['clangd']
    let g:LanguageClient_serverCommands.cpp = ['clangd']
    let g:LanguageClient_serverCommands.objcpp = ['clangd']
else
    echomsg "Cland language server is missing!"
    " Dependencies:
    " - clang
endif

if executable('go-langserver')
    let g:LanguageClient_serverCommands.go = ['go-langserver', '-gocodecompletion']
else
    echomsg "Go language server is missing!"
    " Dependencies:
    " - go-langserver-git
    " - gocode-git
endif

if executable('jdtls')
    let g:LanguageClient_serverCommands.java = ['jdtls']
else
    echomsg "Java language server is missing!"
    " Installation instructions:
    " https://www.reddit.com/r/vim/comments/844bwy/has_anyone_gotten_java_language_server_to_work/e2o5hhr
endif
