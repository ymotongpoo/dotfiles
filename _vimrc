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
colorscheme mycontrast
syntax on
autocmd BufNewFile *.py set filetype=python fenc=utf-8
autocmd BufNewFile *.js set filetype=javascript fenc=utf-8
autocmd BufNewFile *.html set filetype=html fenc=utf-8
autocmd BufNewFile *.go set filetype=go fenc=utf-8
autocmd FileType c set cindent
autocmd FileType c set tabstop=4
autocmd FileType c set omnifunc=ccomplete#Complete
autocmd FileType cpp set cindent
autocmd FileType cpp set tabstop=4
autocmd FileType cpp set omnifunc=cppcomplete#Complete
autocmd FileType python setl autoindent
autocmd FileType python setl smartindent cinwords=if,elif,else,for,while,try,except,finally,def,class
autocmd FileType python setl tabstop=8 expandtab shiftwidth=4 softtabstop=4
autocmd FileType python set omnifunc=pythoncomplete#Complete
autocmd FileType scala set smarttab
autocmd FileType javascript set smarttab
autocmd FileType javascript set tabstop=2 shiftwidth=2 softtabstop=2 expandtab
autocmd FileType html set smarttab
autocmd FileType html set tabstop=2 shiftwidth=2 expandtab

"set tabstop=2 shiftwidth=2 softtabstop=0
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
autocmd StdinReadPre * let s:std_in=1
autocmd VimEnter * if argc() == 0 && !exists("s:std_in") | NERDTree | endif
nnoremap ,d :execute 'NERDTreeToggle ' . getcwd()<CR>
let NERDTreeShowHidden = 1

" zen coding                                                                                    
let g:user_zen_expandabbr_key = '<c-e>'
let g:user_zen_settings = { 'indentation':'  ' }

" set mode line
set modelines=5 
