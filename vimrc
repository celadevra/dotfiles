""""""""""""""""""""
" > Basic Settings
""""""""""""""""""""
set nocompatible
execute pathogen#infect()
filetype plugin indent on
set nobackup
set expandtab
set tabstop=2
set bs=2
""""""""""""""""""""
" > User Interface
""""""""""""""""""""
set guifont=CosmicSansNeueMono:h13
syntax on
color PaperColor-Dark
let g:airline_theme='PaperColor'
let g:airline_powerline_fonts = 1
set laststatus=2
set number
function! AirLineInit()
  let g:airline_section_a = airline#section#create(['mode', ' ', 'branch'])
  let g:airline_section_b = airline#section#create_left(['ffenc', '%f'])
  let g:airline_section_c = airline#section#create(['filetype'])
  let g:airline_section_x = airline#section#create(['tagbar'])
  let g:airline_section_y = airline#section#create_right(['hunks'])
  let g:airline_section_z = airline#section#create_right(['%P', '%l:%c'])
endfunction
autocmd VimEnter * call AirLineInit()
""""""""""""""""""""
" > Completion
""""""""""""""""""""
imap <Tab> <C-P>
set complete=.,b,u,]
set wildmode=longest,list:longest
set completeopt=menu,preview
""""""""""""""""""""
" > File Nav
""""""""""""""""""""
map <C-T> :NERDTreeToggle<CR>
""""""""""""""""""""
" > Tags
""""""""""""""""""""

