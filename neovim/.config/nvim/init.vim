call plug#begin('~/.config/nvim/plugged')

Plug 'morhetz/gruvbox'
Plug 'tpope/vim-fugitive'
Plug 'airblade/vim-gitgutter'
Plug 'neomake/neomake'
Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' }
Plug 'Shougo/echodoc.vim'
Plug 'vim-airline/vim-airline'
Plug 'Shougo/denite.nvim', { 'do': ':UpdateRemotePlugins' }
Plug 'jmcantrell/vim-virtualenv'

call plug#end()
""""""""""""""""""""""
" Look               "
""""""""""""""""""""""
colorscheme gruvbox
set background=dark
let g:gruvbox_italic=1

set number
set relativenumber
set numberwidth=3
""""""""""""""""""""""
" Behaviour          "
""""""""""""""""""""""
set et
set showcmd
" more natural split
set splitbelow
set splitright
set shiftwidth=4
autocmd! BufWritePost * Neomake
call deoplete#enable()
set noshowmode
""" > Denite settings
if executable('ag')
    let g:denite_source_grep_command='ag'
    let g:denite_source_grep_default_opts='--nocolor --nogroup -S -C0'
    let g:denite_source_grep_recursive_opt=''
endif
