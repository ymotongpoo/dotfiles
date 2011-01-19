"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
""""""""""""""""""""""""""""""""""""""" General Settings
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" vi$B8_49%b!<%I(Boff
set nocompatible

" $B8@8l@_Dj(B
set encoding=utf-8
set enc=utf-8
set fenc=utf-8
set fencs=iso-2022-jp,euc-jp,cp932

" $B=q<0@_Dj(B
syntax on
autocmd FileType c set cindent
autocmd FileType c set tabstop=4
autocmd FileType c set omnifunc=ccomplete#Complete
autocmd FileType cpp set cindent
autocmd FileType cpp set tabstop=4
autocmd FileType cpp set omnifunc=cppcomplete#Complete
autocmd FileType py set smarttab 
autocmd FileType py set omnifunc=pythoncomplete#Complete
autocmd FileType py set smartindent cinwords-if,elif,else,for,while,try,except,finally,def,class
autocmd FileType py set tabstop-8 expandtab shiftwidth-4 softtabstop=4
autocmd FileType scala set smarttab
set tabstop=4 shiftwidth=4 softtabstop=0
set autoindent smartindent
set expandtab

" $B8!:w4XO"(B
set wrapscan " EOF$B$^$G8!:w$7$?$i:G=i$KLa$k(B
set ignorecase " $BBgJ8;z(B/$B>.J8;z$r6hJL$7$J$$(B
set smartcase
set incsearch " $B%$%s%/%j%a%s%?%k%5!<%A(B
set hlsearch

if &t_Co > 1
    syntax enable
endif


" $B%U%!%$%k4XO"(B
set autoread
set noswapfile
set nobackup
set nowritebackup
set hidden

" $B%G%#%9%W%l%$(B
set number " $B9THV9fI=<((B
set ruler " $B%k!<%i!<I=<((B
set title " $B%?%$%H%kI=<((B
set showmatch "$BBP1~$9$k3g8L$rI=<((B

"set textwidth=80
set linespace=0
filetype on
set visualbell " $B%S!<%W2;$NJQ$o$j$K%U%i%C%7%e(B
hi CursorIM guifg=black guibg=red " $BF|K\8lF~NO;~$NJQ99(B

" $BA`:n(B
nnoremap gp "*gp

" ChangeLog$B$K4X$9$k@_Dj(B
let g:changelog_timeformat = "%Y-%m-%d"
let g:changelog_username = "ymotongpoo <ymotongpoo@gmail.com>"

" sed$B%b!<%I$G$OF|K\8lF~NO$O(BOFF
inoremap <ESC> <ESC>:set iminsert=0<CR>

" qbuf.vim
let g:qb_hotkey = "<F3>"
nnoremap <F4> :bd<CR>

" NERDTree
nnoremap ,d :execute 'NERDTreeToggle ' . getcwd()<CR>
