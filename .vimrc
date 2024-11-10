set nocompatible " Disable compatibility with vi
set termguicolors

filetype on " Enable type file detection
filetype plugin on " Enable plugins and load plugin for the detected file type.
filetype indent on " Load an indent file for the detected file type.

syntax on " Turn syntax highlighting on.

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
Plug 'morhetz/gruvbox'
Plug 'srcery-colors/srcery-vim'
Plug 'tpope/vim-unimpaired'
Plug 'tpope/vim-vinegar'
Plug 'tpope/vim-surround'
Plug 'tpope/vim-commentary'
Plug 'jiangmiao/auto-pairs'
Plug 'wellle/targets.vim'
Plug 'vim-scripts/argtextobj.vim'
Plug 'bkad/CamelCaseMotion'
Plug 'fcpg/vim-fahrenheit'
Plug 'vim-airline/vim-airline'
Plug 'kien/ctrlp.vim'
Plug 'skywind3000/vim-auto-popmenu'
Plug 'skywind3000/vim-dict'
call plug#end()

colorscheme gruvbox

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
nnoremap <silent> <C-h> 'A<cr>
nnoremap <silent> <C-j> 'B<cr>
nnoremap <silent> <C-k> 'C<cr>
nnoremap <silent> <C-l> 'D<cr>
nnoremap <silent> <leader>h1 'E<cr>
nnoremap <silent> <leader>h2 'F<cr>
nnoremap <silent> <leader>h3 'G<cr>
nnoremap <silent> <leader>h4 'H<cr>
nnoremap <silent> <leader>h5 'I<cr>
nnoremap <silent> <C-e> :marks<cr>

set completeopt=menu,menuone

" Fuzzy File Search
set path+=**
set wildmenu

cnoremap <C-j> <C-p>
cnoremap <C-k> <C-n>

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
let g:apc_enable_ft = {'*': 1, 'text':1, 'markdown':1, 'php':1}

" source for dictionary, current or other loaded buffers, see ':help cpt'
set cpt=.,k,w,b

" don't select the first item.
set completeopt=menu,menuone,noselect

" suppress annoy messages.
set shortmess+=c
