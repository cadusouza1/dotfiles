set nocompatible " Disable compatibility with vi
set termguicolors

filetype on " Enable type file detection
filetype plugin on " Enable plugins and load plugin for the detected file type.
filetype indent on " Load an indent file for the detected file type.

syntax on " Turn syntax highlighting on.

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

set wildmenu " Enable auto completion menu after pressing TAB.

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
Plug 'morhetz/gruvbox'
Plug 'tpope/vim-unimpaired'
Plug 'tpope/vim-vinegar'
Plug 'tpope/vim-surround'
Plug 'tpope/vim-commentary'
Plug 'jiangmiao/auto-pairs'
Plug 'wellle/targets.vim'
Plug 'vim-scripts/argtextobj.vim'
Plug 'bkad/CamelCaseMotion'
call plug#end()

colorscheme gruvbox

nnoremap <leader>so :so %<cr>

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

" Terminal Config
nnoremap <silent> <leader>t :term<cr>
