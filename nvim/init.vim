" set hidden
set showcmd                  " show command in bottom bar
set cursorline               " highlight current line
set wildmenu                 " visual autocomplete for command menu
set showmatch                " highlight matching brace
set laststatus=2             " window will always have a status line
set nobackup
set nowritebackup
set noswapfile
set updatetime=300
set wrap " Wrap lines
set linebreak " Wraps lines a words
set breakindent " Consistent indent of wrapped linex
set expandtab " Use spaces instead of tab
set softtabstop=2 " Number of spaces per tab
set shiftwidth=2   " Number of auto indent spaces
set autoindent " Auto indent
set showtabline=2
set scrolloff=10

" don't give |ins-completion-menu| messages.
set shortmess+=c
set signcolumn=yes " always show signcolumns

set ignorecase

" set mouse=a " Enable the mouse in all modes
"
if (has("termguicolors"))
  set termguicolors " True colors for colorscheme
endif

set background=dark
" Mark 80th column
set colorcolumn=80
" lightline displays the mode already
set noshowmode

call plug#begin('~/.config/nvim/plugged')
Plug 'tpope/vim-sensible' " Sensible defaults
Plug 'itchyny/lightline.vim' " Light configurable statusline
Plug 'mengelbrecht/lightline-bufferline'
Plug 'airblade/vim-rooter'
" Plug 'justinmk/vim-sneak'
" Plug 'lifepillar/vim-solarized8'
Plug 'joshdick/onedark.vim'
Plug 'sheerun/vim-polyglot' " Language packs
Plug 'pangloss/vim-javascript' " Some extra for js configurability
Plug 'airblade/vim-gitgutter'
Plug '~/lib/fzf' " fzf includes a vim command
Plug 'junegunn/fzf.vim' " This extends it with some more commands
Plug 'tpope/vim-repeat'
Plug 'tpope/vim-surround'
Plug 'editorconfig/editorconfig-vim' " Make vim read editorconfig files
Plug 'tpope/vim-commentary' " Toggle comments
Plug 'tpope/vim-fugitive' " Git
Plug 'tpope/vim-unimpaired'
Plug 'jiangmiao/auto-pairs' " Closes brackets etc, it's nice
Plug 'neoclide/coc.nvim', {'branch': 'release'}
Plug 'rbong/vim-flog' " git branch viewer, integrates with fugitive
Plug 'farmergreg/vim-lastplace' " Reopen files at previous cursor position
Plug 'christoomey/vim-tmux-navigator'
call plug#end()

syntax on
colorscheme onedark

let g:javascript_plugin_flow = 1

let g:closetag_filetypes = 'js,javascript.jsx'
let g:closetag_filenames = '*.js'

function! CocCurrentFunction()
    return get(b:, 'coc_current_function', '')
endfunction

function! LightlineGitBlame() abort
  let blame = get(b:, 'coc_git_blame', '')
  return blame
  " return winwidth(0) > 0 ? blame : ''
endfunction

let g:lightline = {
      \ 'colorscheme': 'onedark'
      \ }
let g:lightline.component_function = {
      \ 'gitbranch': 'fugitive#head',
      \ 'cocstatus': 'coc#status',
      \ 'currentfunction': 'CocCurrentFunction',
      \ 'blame': 'LightlineGitBlame'
      \ }
let g:lightline.component_expand = {'buffers': 'lightline#bufferline#buffers'}
let g:lightline.component_type = {'buffers': 'tabsel'}
let g:lightline.active = {
      \ 'left': [
      \   ['mode', 'paste'],
      \   ['cocstatus', 'currentfunction', 'gitbranch', 'blame', 'readonly', 'filename', 'modified']
      \ ],
      \ 'right': [[],['blame']]
      \ }
let g:lightline.tabline = {'left': [['buffers']], 'right': []}

let g:lightline#bufferline#show_number = 1

" Edit vimrc using :Viminit
command! Viminit :e $MYVIMRC

command! Save :mks! ~/.vim/sessions/Prev.vim

" command! -bang -nargs=* Rg
"   \ call fzf#vim#grep(
"   \   'rg --hidden --column --line-number --no-heading --smart-case '.shellescape(<q-args>),
"   \   1,
"   \   fzf#vim#with_preview(),
"   \   <bang>0)

command! -nargs=0 Prettier :call CocAction('runCommand', 'prettier.formatFile')
command! -nargs=0 EslintFix :call CocAction('runCommand', 'eslint.executeAutofix')

let g:fzf_layout = { 'window': 'enew' }

" Highlight symbol under cursor on CursorHold
autocmd CursorHold * silent call CocActionAsync('highlight')

" map f <Plug>Sneak_s
" map F <Plug>Sneak_S

" Remap space key to leader
map <Space> <Leader>

" better ESC
tnoremap <Esc> <C-\><C-n>
inoremap fd <Esc>

nmap <Leader>d :copy .<CR>
vmap <Leader>d :copy '><CR>

nmap <Leader>w :w<CR>
nmap <Leader>fs :w<CR>
nmap <Leader>q :q<CR>
nmap <Leader>ps :cd<Space>
nmap <Leader>s /
nmap <Leader>c :
nmap <Leader><Space> :
nmap <Leader>fed :Viminit<CR>
nmap <Leader>bd :bd<CR>
nmap <Leader>l :b#<CR>
nmap <Leader>wo <C-w>o
nmap <Leader>wD <C-w>o

" Remap VIM 0 to firsT non-blank character
map 0 ^

" Some fzf mappings
noremap <silent> <Leader>j :Files<CR>
" nmap <Leader>k :Buffers<CR>
" nmap <Leader>s :Lines<CR>
noremap <silent> <Leader>f :Rg<CR>

nmap <Leader>b :NERDTreeToggle<CR>

nmap <silent> <Leader>g :Prettier<CR>

" Yank to *
noremap Y "*y
" Make 'Enter' clear search highlight
nnoremap <CR> :noh<CR><CR>

" Find and replace
map <leader>sr :%s///g<left><left>

nmap <silent> <Leader>[ <Plug>(coc-diagnostic-prev)
nmap <silent> <Leader>] <Plug>(coc-diagnostic-next)

" " Remap keys for gotos
nmap <silent> gd <Plug>(coc-definition)
nmap <silent> gy <Plug>(coc-type-definition)
nmap <silent> gi <Plug>(coc-implementation)
nmap <silent> gr <Plug>(coc-references)

" Use K to show documentation in preview window
nnoremap <silent> K :call <SID>show_documentation()<CR>

" command! -nargs=? Fold :call     CocAction('fold', <f-args>)

function! s:show_documentation()
  if (index(['vim','help'], &filetype) >= 0)
    execute 'h '.expand('<cword>')
  else
    call CocAction('doHover')
  endif
endfunction

" Use tab for trigger completion with characters ahead and navigate.
" Use command ':verbose imap <tab>' to make sure tab is not mapped by other plugin.
inoremap <silent><expr> <TAB>
      \ pumvisible() ? "\<C-n>" :
      \ <SID>check_back_space() ? "\<TAB>" :
      \ coc#refresh()
inoremap <expr><S-TAB> pumvisible() ? "\<C-p>" : "\<C-h>"

function! s:check_back_space() abort
  let col = col('.') - 1
  return !col || getline('.')[col - 1]  =~# '\s'
endfunction

" Remap for do codeAction of selected region
function! s:cocActionsOpenFromSelected(type) abort
  execute 'CocCommand actions.open ' . a:type
endfunction
xmap <silent> <leader>a :<C-u>execute 'CocCommand actions.open ' . visualmode()<CR>
nmap <silent> <leader>a :<C-u>set operatorfunc=<SID>cocActionsOpenFromSelected<CR>g@

" Use CTRL-S for selections ranges.
" Requires 'textDocument/ionRange' support of LS, ex: coc-tsserver
nmap <silent> <C-s> <Plug>(coc-range-select)
xmap <silent> <C-s> <Plug>(coc-range-select)

" set viminfo+=!

" if !exists('g:PROJECTS')
"   let g:PROJECTS = {}
" endif

" augroup project_discovery
"   autocmd!
"   autocmd User Fugitive let g:PROJECTS[fnamemodify(fugitive#repo().dir(), ':h')] = 1
" augroup END

" command! -complete=customlist,s:project_complete -nargs=1 Project cd <args>

" function! s:project_complete(lead, cmdline, _) abort
"   let results = keys(get(g:, 'PROJECTS', {}))
"   let regex = substitute(a:lead, '.', '[&].*', 'g')
"   return filter(results, 'v:val =~ regex')
" endfunction
