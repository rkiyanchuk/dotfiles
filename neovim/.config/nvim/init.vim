let $VIM_HOME=fnamemodify($MYVIMRC, ':h')
let $VIM_SITE=$HOME . "/.local/share/nvim/site"
let $VIM_PLUG_URL="https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim"

" {{{ PLUGINS

" Auto install vim-plug plugin manager.
if !filereadable($VIM_HOME . '/autoload/plug.vim')
    if executable('curl')
        silent !curl -fLo $VIM_HOME/autoload/plug.vim --create-dirs $VIM_PLUG_URL
        autocmd VimEnter * PlugInstall --sync | source $MYVIMRC
    else
        echomsg "Install curl to download vim-plug plugin manager!"
    endif
endif

call plug#begin($VIM_HOME . "/plugins")
    Plug 'junegunn/vim-plug'  " Generate :help for vim-plug itself.

    " Basics
    Plug 'jandamm/vim-one'
    Plug 'vim-airline/vim-airline' | Plug 'vim-airline/vim-airline-themes'
    Plug 'sjl/gundo.vim', {'on': 'GundoToggle'}  " Browse change history tree.
    Plug 'scrooloose/nerdtree', {'on': ['NERDTreeToggle', 'NERDTreeFind']}
    Plug 'Shougo/denite.nvim'

    " Editing
    Plug 'SirVer/ultisnips' | Plug 'honza/vim-snippets'
    Plug 'cohama/lexima.vim'
    Plug 'fidian/hexmode'  " Hex editor mode.
    Plug 'chrisbra/Colorizer'

    " Version control
    Plug 'gregsexton/gitv', {'on': ['Gitv']} | Plug 'tpope/vim-fugitive'
    Plug 'mhinz/vim-signify'  " Show diff in signcolumn.

    " Completion
    Plug 'ncm2/ncm2' | Plug 'roxma/nvim-yarp'
    Plug 'ncm2/ncm2-bufword'
    Plug 'ncm2/ncm2-path'
    Plug 'ncm2/ncm2-ultisnips'
    Plug 'ncm2/ncm2-vim-lsp'
    if exists('*nvim_open_win')
        Plug 'ncm2/float-preview.nvim'
    endif

    " Programming
    Plug 'liuchengxu/vista.vim'
    Plug 'prabirshrestha/async.vim'
    Plug 'prabirshrestha/vim-lsp'
    Plug 'elixir-editors/vim-elixir'

    " Enhancements for specific file types
    Plug 'sheerun/vim-polyglot'
    Plug 'google/vim-jsonnet'
    Plug 'smancill/conky-syntax.vim'  " Syntax for .conkyrc.
    Plug 'hashivim/vim-vagrant', {'for': 'Vagrantfile'}
call plug#end()
" }}}

" {{{ PLUGIN SETTINGS

" colorscheme
" -----------

set background=dark
let g:one_allow_italics = 1
colorscheme one

" vim-airline
" -----------

let g:airline_powerline_fonts = 1
let g:airline_left_sep = ''
let g:airline_right_sep = ''
let g:airline_left_alt_sep = ''
let g:airline_right_alt_sep = ''
let g:airline_skip_empty_sections = 1

let g:airline#extensions#tabline#enabled = 1
let g:airline#extensions#tabline#show_buffers = 0
let g:airline#extensions#tabline#show_splits = 0
let g:airline#extensions#tabline#show_tab_type = 0
let g:airline#extensions#tabline#show_tabs = 1
let g:airline#extensions#tabline#tab_min_count = 2
let g:airline#extensions#tabline#tab_nr_type = 1  " Show tab number
let g:airline#extensions#wordcount#enabled = 0

let g:airline_section_y = airline#section#create_right(['%{&fenc!=#""?&fenc:&enc}', '%{&ff}', '%2Bₕ'])
let g:airline_section_z = airline#section#create_right(['☰ %2l:%-2v', '%p%%'])

" gundo.vim
" ---------

let g:gundo_prefer_python3 = 1
let g:gundo_help = 0

inoremap <silent> <leader>3 :GundoToggle<CR>
nnoremap <silent> <leader>3 :GundoToggle<CR>

" nerdtree
" --------

let NERDTreeMinimalUI=1
let NERDTreeIgnore = ['\~$', '\.pyc', '__pycache__', '\.o', '.*\.egg-info']

inoremap <silent> <leader>1 :NERDTreeToggle<CR>
nnoremap <silent> <leader>1 :NERDTreeToggle<CR>
nnoremap <silent> <Leader>1f :NERDTreeFind<CR>

function! NERDTreeToggleCursorcolumn()
    if (bufname("%") =~ "NERD_Tree_")
        setlocal nocursorcolumn
    endif
endfunction
autocmd! BufEnter * call NERDTreeToggleCursorcolumn()


" denite.nvim
" -----------

try
    call denite#custom#var('file/rec', 'command', ['rg', '--files', '--glob', '!.git'])
    call denite#custom#var('grep', 'command', ['rg'])
    call denite#custom#var('grep', 'default_opts', ['-i', '--vimgrep', '--no-heading'])
    call denite#custom#var('grep', 'recursive_opts', [])
    call denite#custom#var('grep', 'pattern_opt', ['--regexp'])
    call denite#custom#var('grep', 'separator', ['--'])
    call denite#custom#var('grep', 'final_opts', [])
    call denite#custom#option('_', 'statusline', v:false)
catch
    echomsg "Denite plugin missing"
endtry

function! s:denite_my_settings() abort
    nnoremap <silent><buffer><expr> <CR> denite#do_map('do_action')
    nnoremap <silent><buffer><expr> p denite#do_map('do_action', 'preview')
    nnoremap <silent><buffer><expr> <C-c> denite#do_map('quit')
    nnoremap <silent><buffer><expr> i denite#do_map('open_filter_buffer')
    nnoremap <silent><buffer><expr> <Space> denite#do_map('toggle_select').'j'
endfunction

augroup DENITE
    autocmd!
    autocmd FileType denite call s:denite_my_settings()
augroup end

nnoremap <silent> <leader>ff :Denite file/rec -auto-resize -smartcase -start-filter<CR>
nnoremap <silent> <leader>fb :Denite buffer -auto-resize<CR>
nnoremap <silent> <leader>fg :Denite grep -auto-resize<CR>
nnoremap <silent> <leader>fr :Denite register -auto-resize<CR>
nnoremap <silent> <leader>fw :DeniteCursorWord file/rec buffer grep<CR>

" ultisnips
" ---------

let g:UltiSnipsExpandTrigger = '<C-\>'
command! UltiSnipsListSnippets :call UltiSnips#ListSnippets()

" lexima.vim
" ----------

" Fix compatibiility with autoclosing completion window with pumvisible()
" See https://github.com/cohama/lexima.vim/issues/65
let g:lexima_no_default_rules = 1
call lexima#set_default_rules()
call lexima#insmode#map_hook('before', '<CR>', '')

" Jump over auto-inserted characters.
" See https://github.com/cohama/lexima.vim/issues/83
inoremap <silent> <C-l> <C-r>=lexima#insmode#leave(1, "")<CR>

" vim-signify
" -----------

let g:signify_vcs_list = ['git', 'hg']
let g:signify_sign_delete            = '−'
let g:signify_sign_delete_first_line = '‾'
let g:signify_sign_change            = '~'
let g:signify_sign_changedelete      = '≂'
let g:signify_sign_show_count = 0

" ncm2
" ----

let g:float_preview#docked = 0
let g:float_preview#max_width = 100

augroup NCM2
    autocmd!
    autocmd BufEnter * call ncm2#enable_for_buffer()
augroup end

function! DisableExtras()
  call nvim_win_set_option(g:float_preview#win, 'cursorline', v:false)
  call nvim_win_set_option(g:float_preview#win, 'cursorcolumn', v:false)
endfunction

autocmd User FloatPreviewWinOpen call DisableExtras()

" vista.vim
" ---------

let g:vista_icon_indent = ["▸ ", ""]
let g:vista_blink = [0, 0]
let g:vista_top_level_blink = [0, 0]

inoremap <silent> <leader>2 :Vista!!<CR>
nnoremap <silent> <leader>2 :Vista!!<CR>

if exists('*nvim_open_win')
    let g:vista_echo_cursor = 1
    let g:vista_echo_cursor_strategy = "floating_win"
endif

autocmd FileType vista,vista_kind call DisableExtras()

" vim-polyglot
" ------------

let g:python_highlight_space_errors = 0

" vim-lsp
" -------

let g:lsp_virtual_text_enabled = 0
let g:lsp_diagnostics_float_cursor = 1
let g:lsp_highlights_enabled = 0
let g:lsp_peek_alignment = "top"
let g:lsp_signs_error = {'text': '✘'}
let g:lsp_signs_warning = {'text': ''}
let g:lsp_signs_information = {'text': ''}
let g:lsp_signs_hint = {'text': 'ﰲ'}

nnoremap <leader>K :LspHover<CR>
nnoremap <leader>ld :LspDefinition<CR>
nnoremap <leader>lD :LspPeekDefinition<CR>
nnoremap <leader>lr :LspReferences<CR>
nnoremap <leader>ldd :LspDocumentDiagnostics<CR>
nnoremap <leader>ls :LspDocumentSymbol<CR>
nnoremap <leader>lw :LspWorkspaceSymbol<CR>
nnoremap <leader>lf :LspDocumentFormat<CR>

let g:vista_c_executive = "vim_lsp"
let g:vista_cpp_executive = "vim_lsp"
let g:vista_python_executive = "vim_lsp"
let g:vista_rust_executive = "vim_lsp"
let g:vista_go_executive = "vim_lsp"
let g:vista_sh_executive = "vim_lsp"
let g:vista_java_executive = "vim_lsp"
let g:vista_elixir_executive = "vim_lsp"

augroup LSP
    autocmd!
    autocmd User lsp_setup call lsp#register_server({
        \ 'name': 'clangd',
        \ 'cmd': {server_info->['clangd', '-background-index']},
        \ 'whitelist': ['c', 'cpp', 'objc', 'objcpp'],
        \ })

    autocmd User lsp_setup call lsp#register_server({
        \ 'name': 'pyls',
        \ 'cmd': {server_info->['pyls']},
        \ 'whitelist': ['python'],
        \ 'workspace_config': {'pyls': {'plugins': {'pydocstyle': {'enabled': v:true},
        \                                           'pycodestyle': {'maxLineLength': 100}}
        \     }
        \   }
        \ })

    autocmd User lsp_setup call lsp#register_server({
        \ 'name': 'rls',
        \ 'cmd': {server_info->['rustup', 'run', 'stable', 'rls']},
        \ 'workspace_config': {'rust': {'clippy_preference': 'on'}},
        \ 'whitelist': ['rust'],
        \ })

    autocmd User lsp_setup call lsp#register_server({
        \ 'name': 'gopls',
        \ 'cmd': {server_info->['gopls', '-mode', 'stdio']},
        \ 'whitelist': ['go'],
        \ })

    autocmd User lsp_setup call lsp#register_server({
        \ 'name': 'bash-language-server',
        \ 'cmd': {server_info->[&shell, &shellcmdflag, 'bash-language-server start']},
        \ 'whitelist': ['sh'],
        \ })
    autocmd User lsp_setup call lsp#register_server({
        \ 'name': 'jdtls',
        \ 'cmd': {server_info->['jdtls']},
        \ 'whitelist': ['java'],
        \ })
    autocmd User lsp_setup call lsp#register_server({
        \ 'name': 'elixir-ls',
        \ 'cmd': {server_info->[&shell, &shellcmdflag, '~/workspace/misc/elixir-ls/release/language_server.sh']},
        \ 'whitelist': ['elixir', 'eelixir'],
        \ })
augroup end
" }}}

" {{{ SETTINGS

set autoread
set backspace=indent,eol,start
set backup
set clipboard+=unnamedplus
set completeopt=menuone,noinsert,noselect
set cursorcolumn
set cursorline
set formatoptions+=r  " Auto-insert current comment leader on Enter.
set hidden  " Hide current buffer when opening new file instead of closing it.
set listchars=tab:→\ ,space:·,trail:·,extends:▶,precedes:◀,nbsp:␣,eol:¬
set matchpairs+=<:>
set mouse=a
set mousehide
set mousemodel=popup_setpos
set nofoldenable
set noshowcmd
set noshowmode  " Mode is already displayed by statusline plugin.
set nowrap
set number
set path+=**  " Search downwards in a directory for `gf`, etc.
set scrolloff=3
set showbreak=↪
set showfulltag  " Show arguments for a function when available, etc.
set sidescrolloff=3
set spelllang=en_us,ru_yo,uk
set splitbelow
set splitright
set termguicolors
set textwidth=79
set title
set undofile
set updatetime=1000
set virtualedit=all
set wildmenu

" Search
set hlsearch
set incsearch
set nowrapscan
set ignorecase
set infercase
set smartcase

" Indentation
set breakindent
set autoindent
set expandtab
set shiftwidth=4
set softtabstop=4
set tabstop=4
set shiftround

" Directories for temp files.
set undodir=$VIM_SITE/undo
set backupdir=$VIM_SITE/backups
set directory=$VIM_SITE/swap  " Directory to store swap files.

" Ensure directories for Vim temp files exist.
for path in [&undodir, &backupdir, &directory]
    if !isdirectory(expand(path))
        call mkdir(expand(path), "p")
    endif
endfor

" Filetype settings
let c_comment_strings = 1
let g:load_doxygen_syntax = 1
let g:tex_flavor = "latex"  " Consider .tex files as LaTeX instead of plainTeX.

" Prevent Neovim from calling Python on startup.
" See https://github.com/neovim/neovim/issues/5728#issuecomment-265454125
let g:python3_host_skip_check = 0
let g:python_host_skip_check = 0
let g:python_host_prog  = '/usr/bin/python'
let g:python3_host_prog = '/usr/bin/python3'

" Fix NERDTree curslorline in Neovim.
" Must be after :set termguicolors
" https://github.com/trevordmiller/nova-vim/issues/106
" https://github.com/neovim/neovim/issues/9019
"highlight NERDTreeFile ctermfg=251

" {{{ MAPPINGS

nnoremap <silent> <leader>V :edit $MYVIMRC<CR>
nnoremap <leader>R :source $MYVIMRC<CR>
nnoremap <leader>s :set spell!<CR>
nnoremap <leader>t :split term://zsh<CR>

" Reset search highlighting by double pressing Esc in normal mode.
nnoremap <Esc><Esc> :nohlsearch<CR>

" Autoclose completion window on Enter.
function! OnEnterPressed()
    return empty(v:completed_item) ? "\<C-y>\<CR>" : "\<C-y>"
endfunction
inoremap <expr> <CR> pumvisible() ? OnEnterPressed() : "\<CR>"

" }}}

" {{{ COMMANDS

" Update plugins.
command! Update :PlugUpdate | PlugUpgrade | source $MYVIMRC

" Commands for merge conflict resolution.
command! MergeLocal :diffget //2
command! MergeRemote :diffget //3

" Remove trailing spaces from file.
function! TrimWhitespace()
    let l:save = winsaveview()
    keeppatterns %s/\s\+$//e
    call winrestview(l:save)
endfunction

command! TrimWhitespace call TrimWhitespace() | :write
" }}}

" {{{ AUTOCOMMANDS

augroup CPP
    autocmd!
    autocmd FileType c,h,cpp,cxx setlocal cindent
    autocmd FileType c,h,cpp,cxx setlocal cinoptions = "h3,l1,g1,t0,i4,+4,(0,w1,W4"
augroup END

augroup PYTHON
  autocmd!
  autocmd FileType python setlocal comments+=b:#:   " sphinx (#:) comments
augroup END

augroup MISC
    autocmd!
    autocmd FileType html,xml,yaml setlocal shiftwidth=2
    autocmd FileType html,xml,yaml setlocal tabstop=2
    " Treat .conf files as .cfg.
    autocmd BufReadPost,BufNewFile *.conf setlocal filetype=cfg
augroup END
" }}}
" }}}
