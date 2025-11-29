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
  -- Themes
  {
    "folke/tokyonight.nvim",
    lazy = false,
    priority = 1000,
    config = function()
      if vim.fn.has('termguicolors') == 1 then
        vim.opt.termguicolors = true
      end
      require("tokyonight").setup({
        style = "storm", -- storm, night, moon, day
        transparent = false,
        terminal_colors = true,
        styles = {
          comments = { italic = true },
          keywords = { italic = false },
          functions = {},
          variables = {},
          sidebars = "dark",
          floats = "dark",
        },
        sidebars = { "qf", "help", "nerdtree" },
        hide_inactive_statusline = false,
        dim_inactive = false,
        lualine_bold = false,
      })
      vim.cmd.colorscheme("tokyonight-storm")
    end,
  },
  {
    "catppuccin/nvim",
    name = "catppuccin",
    lazy = true,
    priority = 900,
    config = function()
      require("catppuccin").setup({
        flavour = "mocha", -- latte, frappe, macchiato, mocha
        transparent_background = false,
        show_end_of_buffer = false,
        -- term_colors = true,
        no_italic = false,
        dim_inactive = {
          enabled = false,
          shade = "dark",
          percentage = 0.15,
        },
        styles = {
          comments = { "italic" },
          conditionals = { "italic" },
        },
        integrations = {
          treesitter = true,
          native_lsp = {
            enabled = true,
          },
          gitsigns = true,
          nerdtree = true,
        },
          color_overrides = {
    latte = {
      rosewater = "#dc8a78",
      flamingo = "#dd7878",
      pink = "#ea76cb",
      mauve = "#8839ef",
      red = "#d20f39",
      maroon = "#e64553",
      peach = "#fe640b",
      yellow = "#df8e1d",
      green = "#40a02b",
      teal = "#179299",
      sky = "#04a5e5",
      sapphire = "#209fb5",
      blue = "#1e66f5",
      lavender = "#7287fd",
      text = "#4b535e",
      subtext1 = "#5b636d",
      subtext0 = "#6b727c",
      overlay2 = "#7b828c",
      overlay1 = "#8c939c",
      overlay0 = "#9ca2ab",
      surface2 = "#abb2ba",
      surface1 = "#bcc1c9",
      surface0 = "#cdd1d8",
      base = "#eff2f6",
      mantle = "#e6eaee",
      crust = "#dee2e7",
    },
    frappe = {
      rosewater = "#f2d5cf",
      flamingo = "#eebebe",
      pink = "#f4b8e4",
      mauve = "#ca9ee6",
      red = "#e78284",
      maroon = "#ea999c",
      peach = "#ef9f76",
      yellow = "#e5c890",
      green = "#a6d189",
      teal = "#81c8be",
      sky = "#99d1db",
      sapphire = "#85c1dc",
      blue = "#8caaee",
      lavender = "#babbf1",
      text = "#c8d4e0",
      subtext1 = "#b6c3cf",
      subtext0 = "#a5b1be",
      overlay2 = "#93a0ad",
      overlay1 = "#848f9c",
      overlay0 = "#727d89",
      surface2 = "#626b77",
      surface1 = "#525a65",
      surface0 = "#414853",
      base = "#303741",
      mantle = "#292f38",
      crust = "#242931",
    },
    macchiato = {
      rosewater = "#f4dbd6",
      flamingo = "#f0c6c6",
      pink = "#f5bde6",
      mauve = "#c6a0f6",
      red = "#ed8796",
      maroon = "#ee99a0",
      peach = "#f5a97f",
      yellow = "#eed49f",
      green = "#a6da95",
      teal = "#8bd5ca",
      sky = "#91d7e3",
      sapphire = "#7dc4e4",
      blue = "#8aadf4",
      lavender = "#b7bdf8",
      text = "#c9d7e5",
      subtext1 = "#b7c4d2",
      subtext0 = "#a4b1be",
      overlay2 = "#929eab",
      overlay1 = "#808b97",
      overlay0 = "#6e7884",
      surface2 = "#5b6470",
      surface1 = "#49525d",
      surface0 = "#363e48",
      base = "#242b35",
      mantle = "#1e252e",
      crust = "#171d26",
    },
    mocha = {
      rosewater = "#f5e0dc",
      flamingo = "#f2cdcd",
      pink = "#f5c2e7",
      mauve = "#cba6f7",
      red = "#f38ba8",
      maroon = "#eba0ac",
      peach = "#fab387",
      yellow = "#f9e2af",
      green = "#a6e3a1",
      teal = "#94e2d5",
      sky = "#89dceb",
      sapphire = "#74c7ec",
      blue = "#89b4fa",
      lavender = "#b4befe",
      text = "#cdd9e5",
      subtext1 = "#b9c6d3",
      subtext0 = "#a4b1bf",
      overlay2 = "#909daa",
      overlay1 = "#7d8794",
      overlay0 = "#6a7480",
      surface2 = "#565e6a",
      surface1 = "#434b55",
      surface0 = "#2f363f",
      base = "#1c222a",
      mantle = "#171c23",
      crust = "#10141b",
    },
  },

      })
    end,
  },

  -- Status line
  {
    "nvim-lualine/lualine.nvim",
    dependencies = { "nvim-tree/nvim-web-devicons" },
    config = function()
      local function get_colors()
        local colors = {
          blue   = '#80a0ff',
          cyan   = '#79dac8',
          black  = '#080808',
          white  = '#c6c6c6',
          red    = '#ff5189',
          violet = '#d183e8',
          grey   = '#303030',
        }

        -- Try to use tokyonight colors if available
        local ok, tokyonight = pcall(require, 'tokyonight.colors')
        if ok then
          local c = tokyonight.setup()
          colors = {
            blue   = c.blue,
            cyan   = c.cyan,
            black  = c.bg_dark,
            white  = c.fg,
            red    = c.red,
            violet = c.purple,
            grey   = c.bg_highlight,
          }
        end

        return colors
      end

      local colors = get_colors()

      -- Custom bubbles theme with pill-shaped sections
      local bubbles_theme = {
        normal = {
          a = { fg = colors.black, bg = colors.violet },
          b = { fg = colors.white, bg = colors.grey },
          c = { fg = colors.white },
        },

        insert = { a = { fg = colors.black, bg = colors.blue } },
        visual = { a = { fg = colors.black, bg = colors.cyan } },
        replace = { a = { fg = colors.black, bg = colors.red } },

        inactive = {
          a = { fg = colors.white, bg = colors.black },
          b = { fg = colors.white, bg = colors.black },
          c = { fg = colors.white },
        },
      }

      require('lualine').setup {
        options = {
          theme = bubbles_theme,
          always_show_tabline = false,
          component_separators = '',
          section_separators = { left = '', right = '' },
        },
        sections = {
          lualine_a = { { 'mode', separator = { left = '' }, right_padding = 2 } },
          lualine_b = { 'filename', 'branch', 'diff' },
          lualine_c = { 'diagnostics' },
          lualine_x = {},
          lualine_y = {
            {
              function()
                local enc = vim.opt.fenc:get()
                if enc == '' then
                  enc = vim.opt.enc:get()
                end
                return enc
              end,
            },
            {
              function()
                return vim.opt.ff:get()
              end,
            },
            {
              function()
                local size = vim.fn.getfsize(vim.fn.expand('%'))
                return size >= 0 and string.format('%2dₕ', size) or ''
              end,
            }
          },
          lualine_z = {
            {
              function()
                return string.format('☰ %2d:%-2d', vim.fn.line('.'), vim.fn.col('.'))
              end,
              separator = { right = '' },
              left_padding = 2,
            },
            {
              function()
                return string.format('%d%%%%', math.floor((vim.fn.line('.') / vim.fn.line('$')) * 100))
              end,
              separator = { right = '' },
            },
          },
        },
        inactive_sections = {
          lualine_a = { 'filename' },
          lualine_b = {},
          lualine_c = {},
          lualine_x = {},
          lualine_y = {},
          lualine_z = { 'location' },
        },
        tabline = {
          lualine_a = {
            {
              'tabs',
              mode = 1,
              separator = { left = '', right = '' },
              -- right_padding = 2,
            }
          },
          lualine_b = {},
          lualine_c = {},
          lualine_x = {},
          lualine_y = {},
          lualine_z = {},
        },
        extensions = {},
      }
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

  -- Hex editor
  {
    "RaafatTurki/hex.nvim",
    config = function()
      require('hex').setup()
    end,
  },
})

-- General settings
vim.opt.showtabline = 1  -- Show tabline only when there are multiple tabs
vim.opt.cmdheight = 0  -- Remove command-line space below statusline
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
