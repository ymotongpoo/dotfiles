"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
""""""""""""""""""""""""""""""""""""""" General Settings
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" vi互換モードoff
set nocompatible

" 言語設定
set encoding=utf-8
set enc=utf-8
set fenc=utf-8
set fencs=iso-2022-jp,euc-jp,cp932

" 書式設定
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

" 検索関連
set wrapscan " EOFまで検索したら最初に戻る
set ignorecase " 大文字/小文字を区別しない
set smartcase
set incsearch " インクリメンタルサーチ
set hlsearch

if &t_Co > 1
    syntax enable
endif


" ファイル関連
set autoread
set noswapfile
set nobackup
set nowritebackup
set hidden

" ディスプレイ
set number " 行番号表示
set ruler " ルーラー表示
set title " タイトル表示
set showmatch "対応する括弧を表示

"set textwidth=80
set linespace=0
filetype on
set visualbell " ビープ音の変わりにフラッシュ
hi CursorIM guifg=black guibg=red " 日本語入力時の変更

" 操作
nnoremap gp "*gp

" ChangeLogに関する設定
let g:changelog_timeformat = "%Y-%m-%d"
let g:changelog_username = "ymotongpoo <ymotongpoo@gmail.com>"

" sedモードでは日本語入力はOFF
inoremap <ESC> <ESC>:set iminsert=0<CR>

" qbuf.vim
let g:qb_hotkey = "<F3>"
nnoremap <F4> :bd<CR>

" NERDTree
nnoremap ,d :execute 'NERDTreeToggle ' . getcwd()<CR>
