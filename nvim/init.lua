---Get the full path to project local executable
---@param name string
---@param context ScopeType
---@return string
local get_local_exec = function(name)
  local current_working_dir = vim.loop.cwd()
  local binpath = string.format('%s/%s/%s', current_working_dir, 'node_modules/.bin', name)

  if vim.fn.filereadable(binpath) == 0 then
    return ''
  end

  return binpath
end

---Get the full path to project local executable
---@param name string
---@return string
local get_global_exec = function(name)
  if vim.fn.executable(name) == 1 then
    return vim.fn.exepath(name)
  else
    -- add_checkhealth_error(name)
    return name
  end
end

---Get the full path to executable, search for project installed
---binary, else search for globally install binary. If no executable
---found, then add to the health check, but post no error
---@param name string
---@param context ScopeType
local get_executable_path = function(name)
  local local_binpath = get_local_exec(name)

  if local_binpath == '' then
    return get_global_exec(name)
  end

  return local_binpath
end

require("packer").startup(function()
  use("wbthomason/packer.nvim") -- Package manager
  use("tpope/vim-sensible")
  use("tpope/vim-fugitive")     -- Git commands in nvim
  use("tpope/vim-commentary")
  use("windwp/nvim-autopairs")
  -- use("b3nj5m1n/kommentary")
  -- UI to select things (files, grep results, open buffers...)
  use({ "nvim-telescope/telescope.nvim", requires = { { "nvim-lua/popup.nvim" }, { "nvim-lua/plenary.nvim" } } })
  use({ 'nvim-telescope/telescope-fzf-native.nvim', run = 'make' })
  use("neovim/nvim-lspconfig") -- Collection of configurations for built-in LSP client
  use("hrsh7th/nvim-compe")    -- Autocompletion plugin
  use("folke/trouble.nvim")    -- For showing diagnostics, references, telescope results, quickfix and location lists

  ------------
  -- THEMES --
  ------------
  use("projekt0n/github-nvim-theme")
  -- use 'joshdick/onedark.vim'
  -- use 'wojciechkepka/vim-github-dark'
  -- use 'Mofiqul/vscode.nvim'
  -- use 'tomasiser/vim-code-dark'

  -- Highlights other uses of the word under the cursor
  use("RRethy/vim-illuminate")
  -- Status line
  use("hoob3rt/lualine.nvim")
  -- Git signs
  use({ "lewis6991/gitsigns.nvim", requires = { "nvim-lua/plenary.nvim" } })
  -- Indent lines
  use({
    "lukas-reineke/indent-blankline.nvim",
    config = function()
      require("indent_blankline").setup({
        -- char = "|",
        buftype_exclude = { "terminal" },
        show_end_of_line = true,
        space_char_blankline = " ",
        show_current_context = true,
      })
    end,
  })

  -- Highlight, edit, and navigate code using a fast incremental parsing library
  use("nvim-treesitter/nvim-treesitter")
  -- Additional textobjects for treesitter
  use("nvim-treesitter/nvim-treesitter-textobjects")
  use("JoosepAlviste/nvim-ts-context-commentstring")

  -- use 'christoomey/vim-tmux-navigator'
  -- use 'prettier/vim-prettier'
  -- use("fladson/vim-kitty")
  -- use("ap/vim-buftabline")
  -- use({
  -- 	"folke/zen-mode.nvim",
  -- 	config = function()
  -- 		require("zen-mode").setup({
  -- 			-- your configuration comes here
  -- 			-- or leave it empty to use the default settings
  -- 			-- refer to the configuration section below langauge
  -- 		})
  -- 	end,
  -- })
  -- -- Dims inacitive portions of the code
  -- use({
  -- 	"folke/twilight.nvim",
  -- 	config = function()
  -- 		require("twilight").setup({
  -- 			-- your configuration comes here
  -- 			-- or leave it empty to use the default settings
  -- 			-- refer to the configuration section below
  -- 		})
  -- 	end,
  -- })
  use({
    "jose-elias-alvarez/null-ls.nvim",
    config = function()
      require("null-ls").setup()
    end,
    requires = { "nvim-lua/plenary.nvim" },
  })
  -- use("dag/vim-fish")
  -- use('alaviss/nim.nvim')
end)

local map = function(mode, target, source, opts)
  vim.api.nvim_set_keymap(mode, target, source, opts or {})
end

-- Incremental live completion
vim.o.inccommand = "nosplit"

-- Make line numbers default
vim.wo.number = true

-- Do not save when switching buffers
vim.o.hidden = true

-- Set highlight on search
vim.o.hlsearch = true

vim.g.noshowmode = true

vim.o.cursorline = true

-- Case insensitive searching UNLESS /C or capital in search
vim.o.ignorecase = true
vim.o.smartcase = true

vim.o.tabstop = 2
vim.o.softtabstop = 2
vim.o.shiftwidth = 2
vim.o.expandtab = true

vim.opt.listchars = {
  space = "⋅",
  eol = "↴",
}

-- Remap space as leader key
map("", "<Space>", "<Nop>", { noremap = true, silent = true })
vim.g.mapleader = " "
vim.g.maplocalleader = " "

-- Set colorscheme (order is important here)
vim.o.termguicolors = false
-- vim.o.termguicolors = true
vim.g.onedark_terminal_italics = 2

vim.opt.swapfile = false

require("github-theme").setup()
vim.cmd("colorscheme github_dark_default")
-- vim.cmd("colorscheme github_light")

require("lualine").setup({
  options = {
    theme = "auto",
  },
  sections = {
    lualine_a = { "mode" },
    lualine_b = { "branch" },
    lualine_c = { "filename" },
    lualine_x = { "encoding", "fileformat", "filetype" },
    lualine_y = { "progress" },
    lualine_z = { "location" },
  },
})

require("nvim-autopairs").setup({})

-- require("kommentary.config").configure_language("default", { prefer_single_line_comments = true })

-- Gitsigns
require("gitsigns").setup({
  signs = {
    add = { hl = "GitGutterAdd", text = "+" },
    change = { hl = "GitGutterChange", text = "~" },
    delete = { hl = "GitGutterDelete", text = "_" },
    topdelete = { hl = "GitGutterDelete", text = "‾" },
    changedelete = { hl = "GitGutterChange", text = "~" },
  },
})

-- Telescope
require("telescope").setup({
  defaults = {
    mappings = {
      i = {
        ["<C-u>"] = false,
        ["<C-d>"] = false,
      },
    },
    -- disable_devicons = true,
    -- color_devicons = false,
  },
})

--Add leader shortcuts
map("n", "<leader><space>", [[<cmd>lua require('telescope.builtin').buffers()<CR>]], { noremap = true, silent = true })
map(
  "n",
  "<leader>j",
  [[<cmd>lua require('telescope.builtin').find_files({previewer = false})<CR>]],
  { noremap = true, silent = true }
)
map("n", "<leader>s", [[<cmd>lua require('telescope.builtin').live_grep()<CR>]], { noremap = true, silent = true })
map(
  "n",
  "<leader>sb",
  [[<cmd>lua require('telescope.builtin').current_buffer_fuzzy_find()<CR>]],
  { noremap = true, silent = true }
)
map("n", "<leader>sh", [[<cmd>lua require('telescope.builtin').help_tags()<CR>]], { noremap = true, silent = true })
-- map('n', '<leader>st', [[<cmd>lua require('telescope.builtin').tags()<CR>]], { noremap = true, silent = true })
-- map('n', '<leader>sd', [[<cmd>lua require('telescope.builtin').grep_string()<CR>]], { noremap = true, silent = true })
-- map('n', '<leader>so', [[<cmd>lua require('telescope.builtin').tags{ only_current_buffer = true }<CR>]], { noremap = true, silent = true })
-- map('n', '<leader>?', [[<cmd>lua require('telescope.builtin').oldfiles()<CR>]], { noremap = true, silent = true })

local nvim_lsp = require("lspconfig")

-- Use an on_attach function to only map the following keys
-- after the language server attaches to the current buffer
local on_attach = function(client, bufnr)
  local function buf_set_keymap(...)
    vim.api.nvim_buf_set_keymap(bufnr, ...)
  end
  local function buf_set_option(...)
    vim.api.nvim_buf_set_option(bufnr, ...)
  end

  -- Enable completion triggered by <c-x><c-o>
  buf_set_option("omnifunc", "v:lua.vim.lsp.omnifunc")

  -- Mappings.
  local opts = { noremap = true, silent = true }

  -- See `:help vim.lsp.*` for documentation on any of the below functions
  buf_set_keymap("n", "gD", "<cmd>lua vim.lsp.buf.declaration()<CR>", opts)
  buf_set_keymap("n", "gd", "<cmd>lua vim.lsp.buf.definition()<CR>", opts)
  buf_set_keymap("n", "K", "<cmd>lua vim.lsp.buf.hover()<CR>", opts)
  buf_set_keymap("n", "gi", "<cmd>lua vim.lsp.buf.implementation()<CR>", opts)
  buf_set_keymap("n", "<C-k>", "<cmd>lua vim.lsp.buf.signature_help()<CR>", opts)
  buf_set_keymap("n", "<space>wa", "<cmd>lua vim.lsp.buf.add_workspace_folder()<CR>", opts)
  buf_set_keymap("n", "<space>wr", "<cmd>lua vim.lsp.buf.remove_workspace_folder()<CR>", opts)
  buf_set_keymap("n", "<space>wl", "<cmd>lua print(vim.inspect(vim.lsp.buf.list_workspace_folders()))<CR>", opts)
  buf_set_keymap("n", "<space>D", "<cmd>lua vim.lsp.buf.type_definition()<CR>", opts)
  buf_set_keymap("n", "<space>rn", "<cmd>lua vim.lsp.buf.rename()<CR>", opts)
  buf_set_keymap("n", "<space>ca", "<cmd>lua vim.lsp.buf.code_action()<CR>", opts)
  buf_set_keymap("n", "gr", "<cmd>lua vim.lsp.buf.references()<CR>", opts)
  buf_set_keymap("n", "<space>e", "<cmd>lua vim.diagnostic.show_line_diagnostics()<CR>", opts)
  buf_set_keymap("n", "<Leader>[", "<cmd>lua vim.diagnostic.goto_prev()<CR>", opts)
  buf_set_keymap("n", "<Leader>]", "<cmd>lua vim.diagnostic.goto_next()<CR>", opts)
  -- buf_set_keymap('n', '<space>q', '<cmd>lua vim.lsp.diagnostic.set_loclist()<CR>', opts)
  buf_set_keymap("n", "<space>g", "<cmd>lua vim.lsp.buf.format({ async = true })<CR>", opts)
end

nvim_lsp.tsserver.setup({
  on_attach = function(client, ...)
    on_attach(client, ...)
    filetypes = { "typescript", "typescriptreact", "typescript.tsx" }
    -- Using null-ls for formatting
    client.server_capabilities.document_formatting = false
  end,
})

local nls = require("null-ls")

nls.setup({
  debounce = 150,
  save_after_format = false,
  sources = {
    -- js
    nls.builtins.formatting.prettier,
    -- nls.builtins.formatting.prettierd,
    nls.builtins.diagnostics.eslint.with({
      command = get_executable_path('eslint')
    }),
    -- nls.builtins.diagnostics.eslint_d,

    -- lua
    nls.builtins.diagnostics.selene.with({
      condition = function(utils)
        return utils.root_has_file("selene.toml")
      end,
    }),
    nls.builtins.formatting.stylua,
    -- misc
    nls.builtins.diagnostics.shellcheck,
    nls.builtins.code_actions.gitsigns,
    -- nls.builtins.diagnostics.misspell,

    -- -- python
    -- nls.builtins.diagnostics.flake8,
    -- nls.builtins.diagnostics.mypy,
    -- nls.builtins.formatting.isort,
    -- nls.builtins.formatting.black
  },
  -- on_init = function(client)
  --   client.config.settings.python.pythonPath = get_python_path(client.config.root_dir)
  -- end
})

-- Treesitter configuration
require('telescope').load_extension('fzf')
-- Parsers must be installed manually via :TSInstall
require("nvim-treesitter.configs").setup({
  highlight = {
    enable = true, -- false will disable the whole extension
  },
  incremental_selection = {
    enable = true,
    keymaps = {
      init_selection = "gnn",
      node_incremental = "grn",
      scope_incremental = "grc",
      node_decremental = "grm",
    },
  },
  indent = {
    enable = true,
  },
  textobjects = {
    select = {
      enable = true,
      lookahead = true, -- Automatically jump forward to textobj, similar to targets.vim
      keymaps = {
        -- You can use the capture groups defined in textobjects.scm
        ["af"] = "@function.outer",
        ["if"] = "@function.inner",
        ["ac"] = "@class.outer",
        ["ic"] = "@class.inner",
      },
    },
    move = {
      enable = true,
      set_jumps = true, -- whether to set jumps in the jumplist
      goto_next_start = {
        ["]m"] = "@function.outer",
        ["]]"] = "@class.outer",
      },
      goto_next_end = {
        ["]M"] = "@function.outer",
        ["]["] = "@class.outer",
      },
      goto_previous_start = {
        ["[m"] = "@function.outer",
        ["[["] = "@class.outer",
      },
      goto_previous_end = {
        ["[M"] = "@function.outer",
        ["[]"] = "@class.outer",
      },
    },
  },
})

vim.o.completeopt = "menuone,noselect"

require("compe").setup({
  enabled = true,
  autocomplete = true,
  debug = false,
  min_length = 1,
  preselect = "enable",
  throttle_time = 80,
  source_timeout = 200,
  resolve_timeout = 800,
  incomplete_delay = 400,
  max_abbr_width = 100,
  max_kind_width = 100,
  max_menu_width = 100,
  documentation = {
    border = { "", "", "", " ", "", "", "", " " }, -- the border option is the same as `|help nvim_open_win|`
    winhighlight = "NormalFloat:CompeDocumentation,FloatBorder:CompeDocumentationBorder",
    max_width = 120,
    min_width = 60,
    max_height = math.floor(vim.o.lines * 0.3),
    min_height = 1,
  },

  source = {
    path = true,
    buffer = true,
    calc = true,
    nvim_lsp = true,
    nvim_lua = true,
    vsnip = true,
    ultisnips = true,
    luasnip = true,
  },
})

map("i", "<TAB>", "compe#confirm({ 'keys': '<CR>', 'select': v:true })", { expr = true })

-- Navigate completion menu with Tab and S-Tab
local t = function(str)
  return vim.api.nvim_replace_termcodes(str, true, true, true)
end

local check_back_space = function()
  local col = vim.fn.col(".") - 1
  return col == 0 or vim.fn.getline("."):sub(col, col):match("%s") ~= nil
end

-- Use (s-)tab to:
--- move to prev/next item in completion menuone
--- jump to prev/next snippet's placeholder
_G.tab_complete = function()
  if vim.fn.pumvisible() == 1 then
    return t("<C-n>")
  elseif vim.fn["vsnip#available"](1) == 1 then
    return t("<Plug>(vsnip-expand-or-jump)")
  elseif check_back_space() then
    return t("<Tab>")
  else
    return vim.fn["compe#complete"]()
  end
end
_G.s_tab_complete = function()
  if vim.fn.pumvisible() == 1 then
    return t("<C-p>")
  elseif vim.fn["vsnip#jumpable"](-1) == 1 then
    return t("<Plug>(vsnip-jump-prev)")
  else
    -- If <S-Tab> is not working in your terminal, change it to <C-h>
    return t("<S-Tab>")
  end
end

map("i", "<Tab>", "v:lua.tab_complete()", { expr = true })
map("s", "<Tab>", "v:lua.tab_complete()", { expr = true })
map("i", "<S-Tab>", "v:lua.s_tab_complete()", { expr = true })
map("s", "<S-Tab>", "v:lua.s_tab_complete()", { expr = true })

map("n", "<leader>fs", [[:w<CR>]], { noremap = true, silent = true })
map("n", "<leader>c", [[:]], { silent = false })
map("n", "<leader>d", [[:copy .<CR>]], { silent = true })
map("v", "<leader>d", [[:copy '><CR>]], { silent = true })
-- Yank to *
map("n", "Y", [["*y]], { noremap = true, silent = true })
-- Remap VIM 0 to first non-blank character
map("", "0", [[^]], {})
-- map('n', '<leader>g', [[:Prettier<CR>]], { silent = true })

map("v", ">", ">gv")
map("v", "<", "<gv")

map("n", "n", "nzz")
map("n", "N", "Nzz")

map("n", "H", "^")
map("n", "L", "$")

map("n", "<Enter>", ":nohl<CR>")

vim.cmd([[ command! Viminit :e $MYVIMRC ]])

local tmux_directions = { h = "L", j = "D", k = "U", l = "R" }

tmux_move = function(direction)
  local current_win = vim.api.nvim_get_current_win()
  vim.cmd("wincmd " .. direction)

  if vim.api.nvim_get_current_win() == current_win then
    vim.fn.system("tmux selectp -" .. tmux_directions[direction])
  end
end

map("n", "<C-h>", ":lua tmux_move('h')<CR>", { silent = true })
map("n", "<C-j>", ":lua tmux_move('j')<CR>", { silent = true })
map("n", "<C-k>", ":lua tmux_move('k')<CR>", { silent = true })
map("n", "<C-l>", ":lua tmux_move('l')<CR>", { silent = true })

-- buftabline.map({ prefix = "<Leader>c", cmd = "bdelete" })
-- buftabline.map({ prefix = "<Leader>v", cmd = "vertical sb" })

-- u.nmap("<C-n>", ":BufPrev<CR>")
-- u.nmap("<C-p>", ":BufNext<CR>")
