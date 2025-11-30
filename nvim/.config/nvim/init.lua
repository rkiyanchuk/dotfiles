-- Inspired by: https://git.io/JYNmy

local IS_REMOTE_HOST = os.getenv("SSH_TTY")            -- ssh
    or vim.loop.fs_stat("/run/host/container-manager") -- systemd-nspawn
    or vim.loop.fs_stat("/.dockerenv")                 -- docker


-- VIM OPTIONS

vim.opt.showtabline = 1 -- Show tabline only when there are multiple tabs
vim.opt.cmdheight = 0   -- Remove command-line space below statusline
vim.opt.autoread = true
vim.opt.backspace = "indent,eol,start"
vim.opt.backup = false
vim.opt.clipboard:append("unnamedplus")
vim.opt.completeopt = "menuone,noinsert,noselect"
vim.opt.cursorline = true
vim.opt.formatoptions:append("r")
vim.opt.hidden = true
vim.opt.lazyredraw = true
vim.opt.synmaxcol = 500
vim.opt.listchars = {
    tab      = "--⇥", -- →
    lead     = "·",
    trail    = "·",
    space    = "·",
    nbsp     = "␣",
    extends  = "⟩", -- ▶
    precedes = "⟨", -- ◀
    eol      = "↲",
}
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
vim.opt.spelllang = "en_us,uk"
vim.opt.splitbelow = true
vim.opt.splitright = true
vim.opt.textwidth = 79
vim.opt.title = true
vim.opt.undofile = true
vim.opt.updatetime = 1000
vim.opt.virtualedit = "all"
vim.opt.wildmode = "longest:full"
vim.opt.winborder = "rounded"

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

-- Filetype settings
vim.g.c_comment_strings = 1
vim.g.load_doxygen_syntax = 1
vim.g.tex_flavor = "latex"
vim.g.python_highlight_space_errors = 0

vim.g.loaded_python3_provider = 0
vim.g.loaded_ruby_provider = 0
vim.g.loaded_node_provider = 0
vim.g.loaded_perl_provider = 0

vim.g.netrw_keepdir = 0
vim.g.netrw_banner = 0
vim.g.netrw_winsize = 15


-- KEMAPS

vim.keymap.set("n", "<Esc>", "<cmd>nohlsearch<CR>", { desc = "Clear search highlights" })
vim.keymap.set('n', '<leader>V', ':edit $MYVIMRC<CR>', { silent = true })


-- PLUGINS

-- Bootstrap the plugin manager, i.e. download the latest version from GitHub
-- if it's not yet downloaded.
local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"
if not vim.loop.fs_stat(lazypath) then
    vim.fn.system({
        "git",
        "clone",
        "--filter=blob:none",
        "--single-branch",
        "https://github.com/folke/lazy.nvim.git",
        lazypath,
    })
end
vim.opt.runtimepath:prepend(lazypath)


require("lazy").setup({
    -- Fast asynchronous completion manager that works with omnicomplete, word
    -- completion and built-in LSP. The priority must be higher than of lspconfig
    -- plugin, because its config updates LSP client capabilities.
    {
        "saghen/blink.cmp",
        priority = 100,
        version = "1.*",
        opts = {
            cmdline = { enabled = false },
            completion = {
                accept = { auto_brackets = { enabled = false } },
                documentation = { auto_show = true },
                list = { selection = { preselect = false, auto_insert = false } },
                menu = {
                    auto_show = false, -- Don't show menu automatically
                    draw = {
                        columns = {
                            { "kind_icon", "label", "label_description", gap = 1 },
                            { "kind" },
                        },
                        components = {
                            kind_icon = {
                                text = function(ctx)
                                    local kind_icon, _, _ = require('mini.icons').get('lsp', ctx.kind)
                                    return kind_icon
                                end,
                            },
                        },
                        snippet_indicator = "~",
                    },
                },
                trigger = {
                    show_on_insert_on_trigger_character = false, -- Don't trigger on typing characters
                },
            },
            keymap = {
                preset = "default",
                ["<C-Space>"] = { "show", "fallback" }, -- Trigger completion with CTRL+Space
                ["<Cr>"] = { "accept", "fallback" },
                ["<Tab>"] = {
                    function(cmp)
                        if cmp.is_visible() then
                            return cmp.select_next()
                        else
                            return false -- Let tabout handle it
                        end
                    end,
                    "fallback"
                },
                ["<S-Tab>"] = {
                    function(cmp)
                        if cmp.is_visible() then
                            return cmp.select_prev()
                        else
                            return false -- Let tabout handle it
                        end
                    end,
                    "fallback"
                },
            },
            signature = { enabled = true },
            sources = {
                providers = { lsp = { fallbacks = {} }, buffer = { score_offset = -50 } },
                min_keyword_length = 3,
            },
        },
    },

    -- A collection of small quality-of-life plugins for Neovim, including
    -- general fuzzy finders for files/symbols/etc., a file explorer,
    -- indentation guides and much more.
    {
        "folke/snacks.nvim",
        priority = 100,
        lazy = false,
        opts = {
            gitbrowse = {
                open = function(url)
                    if IS_REMOTE_HOST then
                        vim.fn.setreg("+", url)
                    else
                        vim.ui.open(url)
                    end
                end
            },
            indent = {
                scope = { enabled = false },
            },
            picker = {
                sources = {
                    explorer = {
                        diagnostics = true,
                        layout = {
                            auto_hide = { "input" },
                        },
                    },
                    treesitter = {
                        filter = {
                            python = { "Class", "Function" },
                        }
                    }
                },
                win = {
                    input = {
                        keys = {
                            ["<Esc>"] = { "close", mode = { "n", "i" } },
                        }
                    },
                    preview = {
                        wo = {
                            number = false,
                            signcolumn = "no",
                        }
                    }
                },
            },
        },
        keys = {
            { "<Leader>e", function() Snacks.explorer({ cwd = vim.fn.expand("%:p:h") }) end, desc = "Open file explorer" },
            { "<Leader>G", function() Snacks.gitbrowse() end,                                desc = "Open git browser",                  mode = { "n", "v" } },
            { "'",         function() Snacks.picker.marks() end,                             desc = "Open marks picker" },
            { "<Leader>f", function() Snacks.picker.git_files({ untracked = true }) end,     desc = "Open file picker" },
            { "<Leader>/", function() Snacks.picker.grep() end,                              desc = "Open search in workspace directory" },
            { "<Leader>b", function() Snacks.picker.buffers() end,                           desc = "Open buffer picker" },
            { "<Leader>.", function() Snacks.picker.grep_word() end,                         desc = "Open search of selection",          mode = { "n", "x" } },
            { "<Leader>'", function() Snacks.picker.resume() end,                            desc = "Open last picker" },
            { "<Leader>?", function() Snacks.picker.commands() end,                          desc = "Open command palette" },
            { "<Leader>d", function() Snacks.picker.diagnostics_buffer() end,                desc = "Open diagnostic picker" },
            { "<Leader>g", function() Snacks.picker.git_status() end,                        desc = "Open changed file picker" },
            { "<Leader>H", function() Snacks.toggle.inlay_hints():toggle() end,              desc = "Toggle inlay hints" },
            { "<Leader>3", function() Snacks.toggle.option("spell"):toggle() end,            desc = "Toggle spelling" },
            { "gd",        function() Snacks.picker.lsp_definitions() end,                   desc = "Goto definition" },
            { "gD",        function() Snacks.picker.lsp_declarations() end,                  desc = "Goto declarations" },
            { "grr",       function() Snacks.picker.lsp_references() end,                    desc = "Goto references" },
            { "gi",        function() Snacks.picker.lsp_implementations() end,               desc = "Goto implementation" },
            { "gy",        function() Snacks.picker.lsp_type_definitions() end,              desc = "Goto type definition" },
            { "<Leader>s", function() Snacks.picker.lsp_symbols() end,                       desc = "Open symbol picker" },
            { "<Leader>S", function() Snacks.picker.lsp_workspace_symbols() end,             desc = "Open workspace symbol picker" },
            { "<Leader>q", function() Snacks.picker.treesitter() end,                        desc = "Open symbol picker" },
        },
    },

    -- Lets you navigate your code with search labels, enhanced character
    -- motions, and Treesitter integration.
    {
        "folke/flash.nvim",
        event = "VeryLazy",
        opts = {
            modes = { char = { enabled = false } },
            prompt = { enabled = false },
        },
        keys = {
            { "s", mode = { "n", "x", "o" }, function() require("flash").jump() end,              desc = "Flash" },
            { "S", mode = { "n", "x", "o" }, function() require("flash").treesitter() end,        desc = "Flash Treesitter" },
            { "r", mode = { "o" },           function() require("flash").remote() end,            desc = "Remote Flash" },
            { "R", mode = { "o", "x" },      function() require("flash").treesitter_search() end, desc = "Treesitter Search" },
        },
    },

    -- LSP and its goodies.
    {
        "neovim/nvim-lspconfig",
        dependencies = { "b0o/SchemaStore.nvim" },
        config = function()
            local servers = {
                "bashls",
                "clangd",
                "cssls",
                "dotls",
                "gopls",
                "html",
                "jsonls",
                "just",
                "lua_ls",
                "marksman",
                "pyright",
                "ruff",
                "rust_analyzer",
                "sourcekit",
                "taplo",
                "ts_ls",
                "typos_lsp",
                "yamlls",
            }

            for _, server_name in ipairs(servers) do
                vim.lsp.enable(server_name)
            end

            vim.lsp.config("pyright", {
                settings = {
                    pyright = {
                        disableOrganizeImports = true,
                    },
                    python = {
                        analysis = {
                            autoImportCompletions = false,
                        },
                    },
                },
            })
            vim.lsp.config("ts_ls", {
                settings = {
                    typescript = {
                        inlayHints = {
                            includeInlayParameterNameHints = "all",
                            includeInlayParameterNameHintsWhenArgumentMatchesName = false,
                            includeInlayFunctionParameterTypeHints = true,
                            includeInlayVariableTypeHints = true,
                            includeInlayPropertyDeclarationTypeHints = true,
                            includeInlayFunctionLikeReturnTypeHints = true,
                            includeInlayEnumMemberValueHints = true,
                        },
                    },
                    javascript = {
                        inlayHints = {
                            includeInlayParameterNameHints = "all",
                            includeInlayParameterNameHintsWhenArgumentMatchesName = false,
                            includeInlayFunctionParameterTypeHints = true,
                            includeInlayVariableTypeHints = true,
                            includeInlayPropertyDeclarationTypeHints = true,
                            includeInlayFunctionLikeReturnTypeHints = true,
                            includeInlayEnumMemberValueHints = true,
                        },
                    },
                },
            })
            vim.lsp.config("lua_ls", {
                settings = {
                    Lua = {
                        hint = {
                            enable = true,
                        },
                    },
                },
            })
            vim.lsp.config("jsonls", {
                settings = {
                    json = {
                        schemas = require("schemastore").json.schemas(),
                        validate = { enable = true },
                    },
                },
            })

            vim.keymap.set("n", "gA", "<Cmd>LspClangdSwitchSourceHeader<Cr>", { desc = "Switch between source/header" })
            vim.keymap.set("n", "gS", "<Cmd>LspClangdShowSymbolInfo<Cr>", { desc = "Show symbol info" })
        end,
    },

    -- Tree-sitter is a parser generator tool and an incremental parsing
    -- library. NeoVim can leverage its functionality in various ways:
    -- semantic syntax highlighting, indentation, navigation, etc.
    {
        "nvim-treesitter/nvim-treesitter",
        lazy = false,
        branch = "main",
        build = ":TSUpdate",
    },

    -- Smart and powerful comment plugin with support for multiple filetypes
    {
        "numToStr/Comment.nvim",
        config = function()
            require("Comment").setup()
            -- Map CMD+/ to toggle comment (works in normal and visual mode)
            vim.keymap.set("n", "<D-/>", "<Plug>(comment_toggle_linewise_current)", { desc = "Toggle comment" })
            vim.keymap.set("v", "<D-/>", "<Plug>(comment_toggle_linewise_visual)", { desc = "Toggle comment" })
        end,
    },
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
        "nvim-lualine/lualine.nvim",
        dependencies = { "stevearc/aerial.nvim" },
        config = function()
            local breadcrump_sep = " ⟩ "
            local format_hl = require("lualine.highlight").component_format_highlight

            local function get_colors()
                local colors = {
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

            require("lualine").setup({
                options = {
                    globalstatus = true,
                    section_separators = { left = '', right = '' },
                    theme = bubbles_theme,
                    always_show_tabline = false,
                    component_separators = { left = '', right = '' },
                },
                sections = {
                    lualine_a = { { 'mode', separator = { left = '' } } },
                    lualine_b = {
                        {
                            "filename",
                            path = 1,
                            fmt = function(str, ctx)
                                local path_separator = package.config:sub(1, 1)
                                local colorized_sep = ""
                                    .. "%" .. format_hl({ name = "lualine_b_aerial_LLNonText" })
                                    .. breadcrump_sep
                                    .. "%" .. ctx.default_hl;
                                return str:gsub(path_separator, colorized_sep);
                            end
                        },
                        {
                            "aerial",
                            sep = breadcrump_sep,
                            sep_prefix = true,
                            padding = { left = 0, right = 1 },
                        },
                    },
                    lualine_c = {},
                    lualine_x = {
                        { "vim.lsp.status():gsub('%%', '%%%%')", icon = "" },
                    },
                    lualine_y = {
                        "diagnostics",
                        {
                            "encoding",
                            cond = function()
                                -- UTF-8 is the de-facto standard encoding and is what
                                -- most users expect by default. There's no need to
                                -- show encoding unless it's something else.
                                local fenc = vim.opt_local.fenc:get()
                                return string.len(fenc) > 0 and string.lower(fenc) ~= "utf-8"
                            end,
                        },
                        "filetype",
                        "fileformat",
                        "progress",
                    },
                    lualine_z = {{ 'location', separator = { right = '' }}},
                },
                tabline = {
                    lualine_a = {
                        {
                            'tabs',
                            mode = 1,
                            separator = { left = '', right = '' },
                        }
                    },
                    lualine_b = {},
                    lualine_c = {},
                    lualine_x = {},
                    lualine_y = {},
                    lualine_z = {},
                },
            })
        end,
    },
    {
        "stevearc/aerial.nvim",
        opts = {
            layout = {
                min_width = 40,
                max_width = 40,
            },
            highlight_on_jump = false,
            close_on_select = true,
            show_guides = true,
        },
        keys = {
            -- { "<Leader>2", "<Cmd>AerialToggle!<Cr>", desc = "Toggle code outline" },
        },
        lazy = false,
    },
    {
        "folke/which-key.nvim",
        opts = {
            preset = "modern",
            filter = function(mapping)
                -- Do not show key mappings w/o description, since it won't be
                -- useful anyway.
                return mapping.desc and mapping.desc ~= ""
            end,
            spec = {
                { "<Leader>h", group = "Git [H]unk" }
            }
        },
    },
    {
        "lewis6991/gitsigns.nvim",
        opts = {
            preview_config = {
                focusable = false,
            },
            on_attach = function(buffer)
                local gitsigns = require("gitsigns")

                -- There's no need for next/prev hunk keymaps for diff buffers
                -- since they support them natively.
                if not vim.wo.diff then
                    vim.keymap.set("n", "]c", gitsigns.next_hunk, { buffer = buffer, desc = "Goto next change" })
                    vim.keymap.set("n", "[c", gitsigns.prev_hunk, { buffer = buffer, desc = "Goto previous change" })
                end

                vim.keymap.set("n", "<Leader>hs", function()
                    gitsigns.stage_hunk()
                end, { buffer = buffer, desc = "Stage hunk under cursor" })

                vim.keymap.set("v", "<Leader>hs", function()
                    gitsigns.stage_hunk({ vim.fn.line("."), vim.fn.line("v") })
                end, { buffer = buffer, desc = "Stage selected hunk" })

                vim.keymap.set("n", "<Leader>hr", function()
                    gitsigns.reset_hunk()
                end, { buffer = buffer, desc = "Revert hunk under cursor" })

                vim.keymap.set("v", "<Leader>hr", function()
                    gitsigns.reset_hunk({ vim.fn.line('.'), vim.fn.line('v') })
                end, { buffer = buffer, desc = "Revert selected hunk" })

                vim.keymap.set("n", "<Leader>hp", function()
                    gitsigns.preview_hunk()
                end, { buffer = buffer, desc = "Show current hunk" })

                vim.keymap.set("n", "<Leader>hb", function()
                    gitsigns.blame_line({ full = true })
                end, { buffer = buffer, desc = "Blame current line" })
            end,
        },
    },

    -- Nerd icons for everything we need.
    {
        "echasnovski/mini.icons",
        opts = {
            lsp = {
                ["function"] = { glyph = "󰊕" },
            },
        },
        config = function(_, opts)
            require("mini.icons").setup(opts)
            MiniIcons.mock_nvim_web_devicons()
        end,
    },

    { "brenoprata10/nvim-highlight-colors", opts = {} },
    { "tpope/vim-sleuth" },
    {
        "williamboman/mason.nvim",
        opts = {},
    },
    {
        "RaafatTurki/hex.nvim",
        config = function()
            require('hex').setup()
        end,
    },
    {
        "cohama/lexima.vim",
        config = function()
            -- Disable lexima's default <Tab> mapping to let tabout handle it
            vim.g.lexima_no_default_rules = 0
            vim.g.lexima_enable_basic_rules = 1
            vim.g.lexima_enable_newline_rules = 1
            vim.g.lexima_enable_endwise_rules = 1
        end,
    },
    {
        -- Tab out of brackets with treesitter.
        'abecodes/tabout.nvim',
        lazy = false,
        config = function()
            require('tabout').setup {
                tabkey = '<Tab>', -- key to trigger tabout, set to an empty string to disable
                backwards_tabkey = '<S-Tab>', -- key to trigger backwards tabout, set to an empty string to disable
                act_as_tab = true, -- shift content if tab out is not possible
                act_as_shift_tab = false, -- reverse shift content if tab out is not possible (if your keyboard/terminal supports <S-Tab>)
                default_tab = '<C-t>', -- shift default action (only at the beginning of a line, otherwise <TAB> is used)
                default_shift_tab = '<C-d>', -- reverse shift default action,
                completion = false, -- if the tabkey is used in a completion pum
                tabouts = {
                    { open = "'", close = "'" },
                    { open = '"', close = '"' },
                    { open = '`', close = '`' },
                    { open = '(', close = ')' },
                    { open = '[', close = ']' },
                    { open = '{', close = '}' },
                    { open = '<', close = '>' }
                },
                opt = true,  -- Set this to true if the plugin is optional
                event = 'InsertCharPre', -- Set the event to 'InsertCharPre' for better compatibility
                ignore_beginning = true, --[[ if the cursor is at the beginning of a filled element it will rather tab out than shift the content ]]
                enable_backwards = true,
                exclude = {}, -- tabout will ignore these filetypes
            }
        end,
        dependencies = { -- These are optional
            "nvim-treesitter/nvim-treesitter",
        },
        priority = 1000,
    },
    {
        -- tabout of brackets without treesitter.
        "kawre/neotab.nvim",
        event = "InsertEnter",
        opts = {
            -- configuration goes here
        },
    }

}, {
    lockfile = vim.fn.stdpath("data") .. "/lazy-lock.json",
    install = { colorscheme = { "tokyonight-storm" } },
    ui = { border = vim.o.winborder },
})


-- CLIPBOARD

-- When running NeoVim over SSH or in a Linux container, system clipboard
-- integration usually fails due to the lack of X11 or Wayland sockets. Using
-- OSC 52 escape codes can improve clipboard integration if supported by the
-- terminal emulator.
-- Source: https://github.com/ikalnytskyi/dotfiles/blob/master/nvim/.config/nvim/init.lua
if IS_REMOTE_HOST then
    local osc52 = require("vim.ui.clipboard.osc52")
    vim.g.clipboard = {
        name = "OSC 52",
        copy = {
            ["+"] = osc52.copy("+"),
            ["*"] = osc52.copy("*"),
        },
        paste = {
            ["+"] = osc52.paste("+"),
            ["*"] = osc52.paste("*"),
        },
    }
end
