call plug#begin('~/.config/nvim/plugged')

Plug 'morhetz/gruvbox'
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
