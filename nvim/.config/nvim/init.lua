-- Inspired by: https://git.io/JYNmy

local IS_REMOTE_HOST = os.getenv("SSH_TTY")           -- ssh
    or vim.uv.fs_stat("/run/host/container-manager")  -- systemd-nspawn
    or vim.uv.fs_stat("/.dockerenv")                  -- docker

-- VIM OPTIONS

vim.opt.showtabline = 1 -- Show tabline only when there are multiple tabs
vim.opt.cmdheight = 0   -- Remove command-line space below statusline
vim.opt.autoread = true
vim.opt.backup = false
vim.opt.clipboard:append("unnamedplus")
vim.opt.cursorline = true
vim.opt.formatoptions:append("r")
-- vim.opt.lazyredraw = true
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
vim.opt.foldenable = false
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

vim.diagnostic.config({
    virtual_text = false,
    virtual_lines = { current_line = true },
    float = { border = "rounded" },
    update_in_insert = false,
})

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
vim.g.tex_flavor = "latex"

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
vim.keymap.set("n", "<D-/>", "gcc", { remap = true, desc = "Toggle comment" })
vim.keymap.set("v", "<D-/>", "gc",  { remap = true, desc = "Toggle comment" })


-- PLUGINS

vim.pack.add({
    { src = 'https://github.com/navarasu/onedark.nvim',              name = 'onedark.nvim' },
    { src = 'https://github.com/echasnovski/mini.icons',             name = 'mini.icons' },
    { src = 'https://github.com/b0o/SchemaStore.nvim',              name = 'SchemaStore.nvim' },

    { src = 'https://github.com/stevearc/aerial.nvim',              name = 'aerial.nvim' },
    { src = 'https://github.com/nvim-lualine/lualine.nvim',         name = 'lualine.nvim' },
    { src = 'https://github.com/saghen/blink.cmp',                  name = 'blink.cmp', version = 'v1' },
    { src = 'https://github.com/folke/snacks.nvim',                 name = 'snacks.nvim' },
    { src = 'https://github.com/neovim/nvim-lspconfig',             name = 'nvim-lspconfig' },
    { src = 'https://github.com/folke/which-key.nvim',              name = 'which-key.nvim' },
    { src = 'https://github.com/lewis6991/gitsigns.nvim',           name = 'gitsigns.nvim' },
    { src = 'https://github.com/akinsho/git-conflict.nvim',         name = 'git-conflict.nvim' },
    { src = 'https://github.com/brenoprata10/nvim-highlight-colors', name = 'nvim-highlight-colors' },
    { src = 'https://github.com/tpope/vim-sleuth',                  name = 'vim-sleuth' },
    { src = 'https://github.com/williamboman/mason.nvim',           name = 'mason.nvim' },
    { src = 'https://github.com/RaafatTurki/hex.nvim',             name = 'hex.nvim' },
    { src = 'https://github.com/echasnovski/mini.pairs',            name = 'mini.pairs' },

    { src = 'https://github.com/nvim-treesitter/nvim-treesitter',   name = 'nvim-treesitter' },

    { src = 'https://github.com/folke/flash.nvim',                  name = 'flash.nvim' },
    { src = 'https://github.com/kawre/neotab.nvim',                name = 'neotab.nvim' },
}, { load = false, confirm = false })


vim.api.nvim_create_user_command("Update", function()
    local unused = vim.iter(vim.pack.get())
        :filter(function(x) return not x.active end)
        :map(function(x) return x.spec.name end)
        :totable()
    if #unused > 0 then vim.pack.del(unused) end
    vim.pack.update()
end, {})

-- Colorscheme first so subsequent plugins inherit correct colors.
vim.cmd.packadd("onedark.nvim")
vim.opt.termguicolors = true
local ok = pcall(function()
    require("onedark").setup({
        style = "dark",
        transparent = true,
        term_colors = true,
        code_style = {
            comments = 'italic',
            keywords = 'none',
            functions = 'none',
            variables = 'none',
        },
        lualine = { transparent = false },
    })
    require("onedark").load()
end)
if not ok then
    vim.cmd.colorscheme("habamax") -- built-in fallback on first install
end
vim.api.nvim_set_hl(0, "StatusLine", { bg = "none" })
vim.api.nvim_set_hl(0, "StatusLineNC", { bg = "none" })
vim.api.nvim_set_hl(0, "SnacksIndent", { fg = "#3e434f" })

-- Icons early — blink.cmp's menu draw function calls require('mini.icons') synchronously.
vim.cmd.packadd("mini.icons")
require("mini.icons").setup({
    lsp = {
        ["function"] = { glyph = "󰊕" },
    },
})
MiniIcons.mock_nvim_web_devicons()

-- SchemaStore — must be on runtimepath before nvim-lspconfig config runs.
vim.cmd.packadd("SchemaStore.nvim")

-- LSP and its goodies.
vim.cmd.packadd("nvim-lspconfig")
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
vim.lsp.inlay_hint.enable(true)

-- Fast asynchronous completion manager. Priority must be higher than lspconfig
-- because its config updates LSP client capabilities.
vim.cmd.packadd("blink.cmp")
require("blink.cmp").setup({
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
            show_on_insert_on_trigger_character = true, -- Don't trigger on typing characters
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
    fuzzy = { implementation = "lua" },
    signature = { enabled = true },
    sources = {
        providers = { lsp = { fallbacks = {} }, buffer = { score_offset = -50 } },
        min_keyword_length = 3,
    },
})

-- A collection of small quality-of-life plugins for Neovim, including
-- general fuzzy finders for files/symbols/etc., a file explorer, and more.
vim.cmd.packadd("snacks.nvim")
require("snacks").setup({
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
})
vim.keymap.set("n",          "<Leader>e", function() Snacks.explorer({ cwd = vim.fn.expand("%:p:h") }) end, { desc = "Open file explorer" })
vim.keymap.set({ "n", "v" }, "<Leader>G",  function() Snacks.gitbrowse() end,                               { desc = "Open git browser" })
vim.keymap.set("n",          "'",          function() Snacks.picker.marks() end,                            { desc = "Open marks picker" })
vim.keymap.set("n",          "<Leader>f",  function() Snacks.picker.git_files({ untracked = true }) end,    { desc = "Open file picker" })
vim.keymap.set("n",          "<Leader>F",  function() Snacks.picker.files() end,                            { desc = "Open file picker (all files)" })
vim.keymap.set("n",          "<Leader>/",  function() Snacks.picker.grep() end,                             { desc = "Open search in workspace directory" })
vim.keymap.set("n",          "<Leader>b",  function() Snacks.picker.buffers() end,                          { desc = "Open buffer picker" })
vim.keymap.set({ "n", "x" }, "<Leader>.",  function() Snacks.picker.grep_word() end,                        { desc = "Open search of selection" })
vim.keymap.set("n",          "<Leader>'",  function() Snacks.picker.resume() end,                           { desc = "Open last picker" })
vim.keymap.set("n",          "<Leader>?",  function() Snacks.picker.commands() end,                         { desc = "Open command palette" })
vim.keymap.set("n",          "<Leader>d",  function() Snacks.picker.diagnostics_buffer() end,               { desc = "Open diagnostic picker" })
vim.keymap.set("n",          "<Leader>g",  function() Snacks.picker.git_status() end,                       { desc = "Open changed file picker" })
vim.keymap.set("n",          "<Leader>H",  function() Snacks.toggle.inlay_hints():toggle() end,             { desc = "Toggle inlay hints" })
vim.keymap.set("n",          "<Leader>3",  function() Snacks.toggle.option("spell"):toggle() end,           { desc = "Toggle spelling" })
vim.keymap.set("n",          "gd",         function() Snacks.picker.lsp_definitions() end,                  { desc = "Goto definition" })
vim.keymap.set("n",          "gD",         function() Snacks.picker.lsp_declarations() end,                 { desc = "Goto declarations" })
vim.keymap.set("n",          "grr",        function() Snacks.picker.lsp_references() end,                   { desc = "Goto references" })
vim.keymap.set("n",          "gi",         function() Snacks.picker.lsp_implementations() end,              { desc = "Goto implementation" })
vim.keymap.set("n",          "gy",         function() Snacks.picker.lsp_type_definitions() end,             { desc = "Goto type definition" })
vim.keymap.set("n",          "<Leader>s",  function() Snacks.picker.lsp_symbols() end,                      { desc = "Open symbol picker" })
vim.keymap.set("n",          "<Leader>S",  function() Snacks.picker.lsp_workspace_symbols() end,            { desc = "Open workspace symbol picker" })


-- Code outline — must load before lualine (used as a lualine component).
vim.cmd.packadd("aerial.nvim")
require("aerial").setup({
    layout = {
        min_width = 40,
        max_width = 40,
    },
    highlight_on_jump = false,
    close_on_select = true,
    show_guides = true,
})

-- Statusline — after aerial and onedark.
vim.cmd.packadd("lualine.nvim")
local breadcrump_sep = " ⟩ "
local format_hl = require("lualine.highlight").component_format_highlight

local colors = {
    blue   = '#61afef',
    cyan   = '#56b6c2',
    black  = '#1e2127',
    white  = '#abb2bf',
    red    = '#e06c75',
    violet = '#c678dd',
    grey   = '#3e4451',
    bg     = '#282c34',
}

-- Custom bubbles theme with pill-shaped sections
local bubbles_theme = {
    normal = {
        a = { fg = colors.black, bg = colors.blue },
        b = { fg = colors.white, bg = colors.grey },
        c = { fg = colors.white, bg = 'none' },
    },

    insert = { a = { fg = colors.black, bg = colors.violet } },
    visual = { a = { fg = colors.black, bg = colors.cyan } },
    replace = { a = { fg = colors.black, bg = colors.red } },

    inactive = {
        a = { fg = colors.white, bg = colors.black },
        b = { fg = colors.white, bg = colors.black },
        c = { fg = colors.white, bg = 'none' },
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
            { "vim.lsp.status():gsub('%%', '%%%%')", icon = "" },
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

-- Keybinding help popup.
vim.cmd.packadd("which-key.nvim")
require("which-key").setup({
    preset = "modern",
    filter = function(mapping)
        -- Do not show key mappings w/o description, since it won't be useful anyway.
        return mapping.desc and mapping.desc ~= ""
    end,
    spec = {
        { "<Leader>h", group = "Git [H]unk" }
    }
})

-- Git signs and hunk operations.
vim.cmd.packadd("gitsigns.nvim")
require("gitsigns").setup({
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
})


-- Merge-conflict resolution: co/ct/cb/c0 to pick ours/theirs/both/none, ]x/[x to navigate.
vim.cmd.packadd("git-conflict.nvim")
require("git-conflict").setup({})

-- Inline color highlighting for CSS/hex color values.
vim.cmd.packadd("nvim-highlight-colors")
require("nvim-highlight-colors").setup({})

-- Auto-detect indentation style from file.
vim.cmd.packadd("vim-sleuth")

-- LSP/DAP/formatter installer.
vim.cmd.packadd("mason.nvim")
require("mason").setup({})

-- Hex viewer/editor.
vim.cmd.packadd("hex.nvim")
require('hex').setup()

-- Auto-pairing of brackets, quotes, etc.
vim.cmd.packadd("mini.pairs")
require("mini.pairs").setup()

-- Syntax highlighting, indentation, and code-aware text objects.
-- Run :Update then :TSUpdate after first install to download parsers.
pcall(function()
    vim.cmd.packadd("nvim-treesitter")
    require("nvim-treesitter.configs").setup({
        highlight = { enable = true },
        indent = { enable = true },
    })
end)

-- Lets you navigate your code with search labels and enhanced character motions.
vim.api.nvim_create_autocmd("VimEnter", {
    once = true,
    callback = function()
        vim.cmd.packadd("flash.nvim")
        require("flash").setup({
            modes = { char = { enabled = false } },
            prompt = { enabled = false },
        })
        vim.keymap.set({ "n", "x", "o" }, "s", function() require("flash").jump() end,   { desc = "Flash" })
        vim.keymap.set("o",               "r", function() require("flash").remote() end, { desc = "Remote Flash" })
    end,
})

-- Tabout of brackets. Loaded on first insert.
vim.api.nvim_create_autocmd("InsertEnter", {
    once = true,
    callback = function()
        vim.cmd.packadd("neotab.nvim")
        require("neotab").setup({})
    end,
})


-- INPUT METHOD

-- Restore the non-ASCII input method on InsertEnter, switch back to ASCII on
-- InsertLeave, so normal-mode commands always work regardless of system layout.
do
    local default_im = "com.apple.keylayout.PolishPro"
    local im_select = "/opt/homebrew/bin/im-select"
    local im_current = default_im
    vim.api.nvim_create_autocmd("InsertLeave", {
        callback = function()
            vim.fn.jobstart({ im_select }, {
                stdout_buffered = true,
                on_stdout = function(_, data)
                    local im = vim.trim(table.concat(data))
                    if im ~= "" then im_current = im end
                    vim.fn.jobstart({ im_select, default_im })
                end,
            })
        end,
    })
    vim.api.nvim_create_autocmd("InsertEnter", {
        callback = function()
            if im_current ~= default_im then
                vim.fn.jobstart({ im_select, im_current })
            end
        end,
    })
end


-- COMMIT MESSAGE GENERATION

-- In a gitcommit buffer, <Leader>c (or :CommitMsg) inserts a Claude-generated
-- commit message at the top of the buffer based on the currently staged diff.
-- The call runs asynchronously so the editor stays responsive; a Braille
-- spinner is shown as virtual text at line 1 to indicate progress.
vim.api.nvim_create_autocmd("FileType", {
    pattern = "gitcommit",
    callback = function(args)
        local ns = vim.api.nvim_create_namespace("claude_commit_spinner")
        local frames = { "⠋", "⠙", "⠹", "⠸", "⠼", "⠴", "⠦", "⠧", "⠇", "⠏" }
        local prompt = "Write git commit message for staged changes. Return *only* the body of the commit message, do not wrap in code fences or backticks"

        local function generate()
            local diff = vim.fn.system("git diff --staged")
            if diff == "" then
                vim.notify("No staged changes", vim.log.levels.WARN)
                return
            end

            local bufnr = vim.api.nvim_get_current_buf()
            local idx = 1
            local function spinner_extmark(id)
                return vim.api.nvim_buf_set_extmark(bufnr, ns, 0, 0, {
                    id = id,
                    virt_text = { { frames[idx] .. " Generating commit message…", "Comment" } },
                    virt_text_pos = "eol",
                })
            end
            local mark_id = spinner_extmark(nil)
            local timer = vim.uv.new_timer()
            timer:start(100, 100, vim.schedule_wrap(function()
                idx = (idx % #frames) + 1
                if vim.api.nvim_buf_is_valid(bufnr) then spinner_extmark(mark_id) end
            end))

            vim.system({ "claude", "-p", prompt }, { stdin = diff }, function(result)
                vim.schedule(function()
                    timer:stop()
                    timer:close()
                    if vim.api.nvim_buf_is_valid(bufnr) then
                        vim.api.nvim_buf_del_extmark(bufnr, ns, mark_id)
                    end
                    if result.code ~= 0 then
                        vim.notify("claude failed: " .. (result.stderr or ""), vim.log.levels.ERROR)
                        return
                    end
                    local msg = vim.trim(result.stdout or "")
                    if msg == "" then
                        vim.notify("claude returned empty message", vim.log.levels.ERROR)
                        return
                    end
                    if vim.api.nvim_buf_is_valid(bufnr) then
                        vim.api.nvim_buf_set_lines(bufnr, 0, 0, false, vim.split(msg, "\n"))
                    end
                end)
            end)
        end

        vim.keymap.set("n", "<Leader>c", generate,
            { buffer = args.buf, desc = "Generate commit message with Claude" })
        vim.api.nvim_buf_create_user_command(args.buf, "CommitMsg", generate,
            { desc = "Generate commit message with Claude" })
    end,
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
