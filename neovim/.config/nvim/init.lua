-- Global variables
vim.g.VIM_HOME = vim.fn.fnamemodify(vim.env.MYVIMRC, ':h')
vim.g.VIM_SITE = vim.env.HOME .. "/.local/share/nvim/site"

-- Bootstrap lazy.nvim
local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"
if not vim.loop.fs_stat(lazypath) then
  vim.fn.system({
    "git",
    "clone",
    "--filter=blob:none",
    "https://github.com/folke/lazy.nvim.git",
    "--branch=stable",
    lazypath,
  })
end
vim.opt.rtp:prepend(lazypath)

-- Plugin setup
require("lazy").setup({
  -- Theme
  {
    "projekt0n/github-nvim-theme",
    lazy = false,
    priority = 1000,
    config = function()
      if vim.fn.has('termguicolors') == 1 then
        vim.opt.termguicolors = true
      end
      vim.opt.background = "dark"
      require('github-theme').setup({
        options = {
          transparent = false,
        },
        palettes = {
          github_dark_dimmed = {
            canvas = {
              default = '#1C2128',
            },
          },
        },
        groups = {
          github_dark_dimmed = {
            Normal = { bg = '#1C2128' },
            NormalFloat = { bg = '#1C2128' },
          },
        },
      })
      vim.cmd.colorscheme("github_dark_dimmed")
    end,
  },

  -- Status line
  {
    "vim-airline/vim-airline",
    dependencies = { "vim-airline/vim-airline-themes" },
    config = function()
      vim.g.airline_theme = 'base16'
      vim.g.airline_powerline_fonts = 1
      vim.g.airline_left_sep = ''
      vim.g.airline_right_sep = ''
      vim.g.airline_left_alt_sep = ''
      vim.g.airline_right_alt_sep = ''
      vim.g.airline_skip_empty_sections = 1

      vim.g['airline#extensions#tabline#enabled'] = 1
      vim.g['airline#extensions#tabline#show_buffers'] = 0
      vim.g['airline#extensions#tabline#show_splits'] = 0
      vim.g['airline#extensions#tabline#show_tab_type'] = 0
      vim.g['airline#extensions#tabline#show_tabs'] = 1
      vim.g['airline#extensions#tabline#tab_min_count'] = 2
      vim.g['airline#extensions#tabline#tab_nr_type'] = 1
      vim.g['airline#extensions#wordcount#enabled'] = 0

      vim.g.airline_section_y = vim.fn['airline#section#create_right']({'%{&fenc!=#""?&fenc:&enc}', '%{&ff}', '%2Bₕ'})
      vim.g.airline_section_z = vim.fn['airline#section#create_right']({'☰ %2l:%-2v', '%p%%'})
    end,
  },

  -- File explorer
  {
    "scrooloose/nerdtree",
    cmd = { "NERDTreeToggle", "NERDTreeFind" },
    config = function()
      vim.g.NERDTreeMinimalUI = 1
      vim.g.NERDTreeIgnore = {'\\~$', '\\.pyc', '__pycache__', '\\.o', '.*\\.egg-info', '.DS_Store'}
    end,
  },

  -- Auto-pairing
  "cohama/lexima.vim",

  -- Commenting
  "tpope/vim-commentary",

  -- Hex mode
  "fidian/hexmode",

  -- Color highlighting
  "chrisbra/Colorizer",

  -- Treesitter
  {
    "nvim-treesitter/nvim-treesitter",
    build = ":TSUpdate",
  },

  -- Git integration
  {
    "tpope/vim-fugitive",
    dependencies = { "tpope/vim-rhubarb" },
  },
  {
    "gregsexton/gitv",
    cmd = "Gitv",
  },
  {
    "mhinz/vim-signify",
    config = function()
      vim.g.signify_vcs_list = {'git', 'hg'}
      vim.g.signify_sign_delete = '−'
      vim.g.signify_sign_delete_first_line = '‾'
      vim.g.signify_sign_change = '~'
      vim.g.signify_sign_changedelete = '≂'
      vim.g.signify_sign_show_count = 0
    end,
  },

  -- LSP
  {
    "prabirshrestha/vim-lsp",
    dependencies = { "prabirshrestha/async.vim" },
    config = function()
      vim.g.lsp_diagnostics_echo_cursor = 0
      vim.g.lsp_diagnostics_float_cursor = 0
      vim.g.lsp_diagnostics_highlights_enabled = 0
      vim.g.lsp_diagnostics_signs_enabled = 0
      vim.g.lsp_diagnostics_virtual_text_enabled = 0
      vim.g.lsp_highlights_enabled = 0
      vim.g.lsp_peek_alignment = "top"
      vim.g.lsp_semantic_enabled = 1
    end,
  },

  -- Language support
  "sheerun/vim-polyglot",
  "google/vim-jsonnet",
  "mustache/vim-mustache-handlebars",
  "dag/vim-fish",
})

-- General settings
vim.opt.autoread = true
vim.opt.backspace = "indent,eol,start"
vim.opt.backup = true
vim.opt.clipboard:append("unnamedplus")
vim.opt.completeopt = "menuone,noinsert,noselect"
vim.opt.cursorline = true
vim.opt.formatoptions:append("r")
vim.opt.hidden = true
vim.opt.lazyredraw = true
vim.opt.synmaxcol = 500
vim.opt.listchars = "tab:→ ,space:·,trail:·,extends:▶,precedes:◀,nbsp:␣,eol:¬"
vim.opt.matchpairs:append("<:>")
vim.opt.mouse = "a"
vim.opt.mousehide = true
vim.opt.mousemodel = "popup_setpos"
vim.opt.foldenable = false
vim.opt.showcmd = false
vim.opt.showmode = false
vim.opt.wrap = false
vim.opt.number = true
vim.opt.path:append("**")
vim.opt.scrolloff = 3
vim.opt.showbreak = "↪"
vim.opt.showfulltag = true
vim.opt.sidescrolloff = 3
vim.opt.signcolumn = "yes"
vim.opt.spelllang = "en_us,ru_yo,uk"
vim.opt.splitbelow = true
vim.opt.splitright = true
vim.opt.textwidth = 79
vim.opt.title = true
vim.opt.undofile = true
vim.opt.updatetime = 1000
vim.opt.virtualedit = "all"
vim.opt.wildmode = "longest:full"

-- Search settings
vim.opt.hlsearch = true
vim.opt.incsearch = true
vim.opt.wrapscan = false
vim.opt.ignorecase = true
vim.opt.infercase = true
vim.opt.smartcase = true

-- Indentation
vim.opt.breakindent = true
vim.opt.autoindent = true
vim.opt.expandtab = true
vim.opt.shiftwidth = 4
vim.opt.softtabstop = 4
vim.opt.tabstop = 4
vim.opt.shiftround = true

-- Directories for temp files
vim.opt.undodir = vim.g.VIM_SITE .. "/undo"
vim.opt.backupdir = vim.g.VIM_SITE .. "/backups"
vim.opt.directory = vim.g.VIM_SITE .. "/swap"

-- Filetype settings
vim.g.c_comment_strings = 1
vim.g.load_doxygen_syntax = 1
vim.g.tex_flavor = "latex"
vim.g.python_highlight_space_errors = 0

-- Python host configuration
vim.g.python3_host_skip_check = 0
vim.g.python_host_skip_check = 0
if vim.fn.has("mac") == 1 then
  if vim.fn.filereadable("/usr/local/bin/python3") == 1 then
    vim.g.python_host_prog = '/usr/local/bin/python'
    vim.g.python3_host_prog = '/usr/local/bin/python3'
  else
    vim.g.python_host_prog = '/opt/homebrew/bin/python'
    vim.g.python3_host_prog = '/opt/homebrew/bin/python3'
  end
else
  vim.g.python_host_prog = '/usr/bin/python'
  vim.g.python3_host_prog = '/usr/bin/python3'
end

-- General mappings
vim.keymap.set('n', '<leader>V', ':edit $MYVIMRC<CR>', { silent = true })
vim.keymap.set('n', '<leader>R', ':source $MYVIMRC<CR>')
vim.keymap.set('n', '<leader>t', ':split term://fish<CR>')
vim.keymap.set('n', 'ZD', ':bdelete<CR>', { silent = true })
vim.keymap.set('n', 'ZB', ':w<CR>:bdelete<CR>', { silent = true })
vim.keymap.set('n', '<Esc><Esc>', ':nohlsearch<CR>')

-- Plugin mappings
vim.keymap.set('n', '<leader>1', ':NERDTreeToggle<CR>', { silent = true })
vim.keymap.set('n', '<leader>!', ':NERDTreeFind<CR>', { silent = true })

-- LSP mappings
vim.keymap.set('n', '<leader>K', ':LspHover<CR>')
vim.keymap.set('n', '<leader>ld', ':LspDefinition<CR>')
vim.keymap.set('n', '<leader>lD', ':LspDeclaration<CR>')
vim.keymap.set('n', '<leader>lr', ':LspReferences<CR>')
vim.keymap.set('n', '<leader>4', ':LspDocumentDiagnostics<CR>')
vim.keymap.set('n', '<leader>lf', ':LspDocumentFormat<CR>')

-- Lexima mapping
vim.keymap.set('i', '<C-l>', '<C-r>=lexima#insmode#leave(1, "")<CR>', { silent = true })

-- Completion mapping
vim.keymap.set('i', '<CR>', function()
  if vim.fn.pumvisible() == 1 then
    return vim.fn.empty(vim.v.completed_item) == 1 and '<C-y><CR>' or '<C-y>'
  else
    return '<CR>'
  end
end, { expr = true })

-- Functions
local function trim_whitespace()
  local save = vim.fn.winsaveview()
  vim.cmd('keeppatterns %s/\\s\\+$//e')
  vim.fn.winrestview(save)
end

local function nerdtree_toggle_cursorcolumn()
  if vim.fn.bufname("%"):match("NERD_Tree_") then
    vim.opt_local.cursorcolumn = false
  end
end

local function disable_extras()
  if vim.g.float_preview and vim.g.float_preview.win then
    vim.api.nvim_win_set_option(vim.g.float_preview.win, 'cursorcolumn', false)
    vim.api.nvim_win_set_option(vim.g.float_preview.win, 'cursorline', false)
    vim.api.nvim_win_set_option(vim.g.float_preview.win, 'number', false)
    vim.api.nvim_win_set_option(vim.g.float_preview.win, 'relativenumber', false)
  end
end

-- Commands
vim.api.nvim_create_user_command('Update', function()
  vim.cmd('Lazy update')
  vim.cmd('source $MYVIMRC')
end, {})

vim.api.nvim_create_user_command('MergeLocal', 'diffget 1', {})
vim.api.nvim_create_user_command('MergeRemote', 'diffget 2', {})
vim.api.nvim_create_user_command('TrimWhitespace', function()
  trim_whitespace()
  vim.cmd('write')
end, {})

-- Autocommands
vim.api.nvim_create_augroup('CPP', { clear = true })
vim.api.nvim_create_autocmd('FileType', {
  group = 'CPP',
  pattern = { 'c', 'h', 'cpp', 'cxx' },
  callback = function()
    vim.opt_local.cindent = true
    vim.opt_local.cinoptions = "h3,l1,g1,t0,i4,+4,(0,w1,W4"
  end,
})

vim.api.nvim_create_augroup('PYTHON', { clear = true })
vim.api.nvim_create_autocmd('FileType', {
  group = 'PYTHON',
  pattern = 'python',
  callback = function()
    vim.opt_local.comments:append('b:#:')
  end,
})

vim.api.nvim_create_augroup('MISC', { clear = true })
vim.api.nvim_create_autocmd('FileType', {
  group = 'MISC',
  pattern = { 'html', 'xml', 'yaml' },
  callback = function()
    vim.opt_local.shiftwidth = 2
    vim.opt_local.tabstop = 2
  end,
})

vim.api.nvim_create_autocmd({ 'BufReadPost', 'BufNewFile' }, {
  group = 'MISC',
  pattern = '*.conf',
  callback = function()
    vim.opt_local.filetype = 'cfg'
  end,
})

vim.api.nvim_create_autocmd('FileType', {
  group = 'MISC',
  pattern = { 'go', 'elixir' },
  callback = function()
    vim.api.nvim_create_autocmd('BufWritePre', {
      buffer = 0,
      command = 'LspDocumentFormatSync',
    })
  end,
})

vim.api.nvim_create_autocmd('BufEnter', {
  callback = nerdtree_toggle_cursorcolumn,
})

vim.api.nvim_create_autocmd('User', {
  pattern = 'FloatPreviewWinOpen',
  callback = disable_extras,
})

-- Float preview settings
vim.g.float_preview = { docked = 0, max_width = 100 }
