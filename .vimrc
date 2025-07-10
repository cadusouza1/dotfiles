set ttyfast

set nocompatible " Disable compatibility with vi
set termguicolors

set autoread

filetype on " Enable type file detection
filetype plugin on " Enable plugins and load plugin for the detected file type.
filetype indent on " Load an indent file for the detected file type.

syntax on

set showcmd

set relativenumber
set number 

set splitright
set splitbelow
set hidden

set nohlsearch " Remove search highligth
set incsearch " incremental search

set expandtab " converts tabs to white space
set shiftwidth=4 " for autoindents
set softtabstop=4 " see multiple spaces as tabstops so <BS> does the right thing
set tabstop=4 " number of columns occupied by a tab

set autoindent " indent a new line the same amount as the line just typed
set scrolloff=999

set background=dark

let g:mapleader=" "

let data_dir = has('nvim') ? stdpath('data') . '/site' : '~/.vim'
if empty(glob(data_dir . '/autoload/plug.vim'))
  silent execute '!curl -fLo '.data_dir.'/autoload/plug.vim --create-dirs  https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim'
  autocmd VimEnter * PlugInstall --sync | source $MYVIMRC
endif

" Install vim-plug if not found
if empty(glob('~/.vim/autoload/plug.vim'))
  silent !curl -fLo ~/.vim/autoload/plug.vim --create-dirs
    \ https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
endif

" Run PlugInstall if there are missing plugins
autocmd VimEnter * if len(filter(values(g:plugs), '!isdirectory(v:val.dir)'))
  \| PlugInstall --sync | source $MYVIMRC
\| endif

call plug#begin()
Plug 'sillybun/vim-repl'
Plug 'morhetz/gruvbox'
Plug 'srcery-colors/srcery-vim'
Plug 'fcpg/vim-fahrenheit'
Plug 'tpope/vim-unimpaired'
Plug 'tpope/vim-vinegar'
Plug 'tpope/vim-surround'
Plug 'tpope/vim-commentary'
Plug 'tpope/vim-repeat'
" Plug 'jiangmiao/auto-pairs'
Plug 'wellle/targets.vim'
" Plug 'vim-scripts/argtextobj.vim'
Plug 'bkad/CamelCaseMotion'
Plug 'vim-airline/vim-airline'
Plug 'kien/ctrlp.vim'
Plug 'skywind3000/vim-auto-popmenu'
Plug 'skywind3000/vim-dict'
Plug 'kovetskiy/sxhkd-vim'
Plug 'mechatroner/rainbow_csv'
call plug#end()

colorscheme gruvbox

nnoremap <leader>r :REPLToggle<cr>
let g:repl_program = { 'default': ['python3.13'] }

nnoremap <leader>ck :checktime<cr>

nnoremap <leader>so :so %<cr>

nnoremap <silent> <Bslash>j :bn<cr>
nnoremap <silent> <Bslash>k :bp<cr>

" CamelCaseMotion Config
map <silent> <leader>w <Plug>CamelCaseMotion_w
map <silent> <leader>b <Plug>CamelCaseMotion_b
map <silent> <leader>e <Plug>CamelCaseMotion_e
map <silent> g<leader>e <Plug>CamelCaseMotion_ge
omap <silent> i<leader>w <Plug>CamelCaseMotion_iw
xmap <silent> i<leader>w <Plug>CamelCaseMotion_iw
omap <silent> i<leader>b <Plug>CamelCaseMotion_ib
xmap <silent> i<leader>b <Plug>CamelCaseMotion_ib
omap <silent> i<leader>e <Plug>CamelCaseMotion_ie
xmap <silent> i<leader>e <Plug>CamelCaseMotion_ie

" Crude simulation of harpoon
nnoremap <silent> <C-h> :b 1<cr>
nnoremap <silent> <C-j> :b 2<cr>
nnoremap <silent> <C-k> :b 3<cr>
nnoremap <silent> <C-l> :b 4<cr>
nnoremap <silent> <leader>h1 :b 5<cr>
nnoremap <silent> <leader>h2 :b 6<cr>
nnoremap <silent> <leader>h3 :b 7<cr>
nnoremap <silent> <leader>h4 :b 8<cr>
nnoremap <silent> <leader>h5 :b 9<cr>
nnoremap <silent> <C-e> :buffers<cr>

nnoremap Y y$

" Fuzzy File Search
set path+=**
set wildmenu

" Tags
command! MakeTags !ctags -R .

" File Browsing
let g:netrw_banner=0
let g:netrw_browse_split=4
let g:netrw_altv=1
let g:netrw_liststyle=3

" this makes vim startup really slow
" let g:netrw_list_hide=netrw_gitignore#Hide() 

" Snippets
nnoremap \mainc :-1read $HOME/.vim/snippets/.skeleton.main.c<cr>j

" vim-auto-popmenu

" enable this plugin for filetypes, '*' for all files.
let g:apc_enable_ft = {'*': 1}

" see ':help cpt'
set cpt=.,w,b,u,t,i
" set cpt=.,k,w,b

" don't select the first item.
set completeopt=menu,menuone,noselect

" suppress annoy messages.
set shortmess+=c

" autocmds
"" formatters
if executable("black")
    augroup PythonFormatter
        autocmd!
        au BufWritePost *.py :silent :!black -q -l 80 %
    augroup END
endif

if executable("prettier")
    augroup PrettierFormatter
        autocmd!
        au BufWritePost *.ts,*.js,*.tsx,*.jsx,*.json,*.html,*.css,*.md :silent :!prettier --tab-width 4 --log-level silent -w %
    augroup END
endif

if executable("clang-format")
    augroup ClangFormat
        autocmd!
        au BufWritePost *.c,*.cpp :silent :!clang-format --style="{IndentWidth: 4}" -i %
    augroup END
endif

hi SpellBad cterm=underline
