call plug#begin('~/.config/nvim/plugged')

Plug 'morhetz/gruvbox'
Plug 'tpope/vim-fugitive'
Plug 'airblade/vim-gitgutter'
Plug 'neomake/neomake'
call plug#end()
""""""""""""""""""""""
" Look               "
""""""""""""""""""""""
colorscheme gruvbox
set background=dark
let g:gruvbox_italic=1
""""""""""""""""""""""
" Behaviour          "
""""""""""""""""""""""
set et
set shiftwidth=4
