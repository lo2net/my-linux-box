set nocompatible               " be iMproved
filetype off                   " required!

set rtp+=~/.vim/bundle/vundle/
call vundle#rc()

" let Vundle manage Vundle
" required!
Bundle 'gmarik/vundle'

" My Bundles here:
"
" original repos on github
Bundle 'tpope/vim-fugitive'
"Bundle 'Lokaltog/vim-easymotion'
"Bundle 'rstacruz/sparkup', {'rtp': 'vim/'}

if !has("win32")
Bundle 'Valloric/YouCompleteMe'
endif

" vim-scripts repos
Bundle 'L9'
Bundle 'FuzzyFinder'
"Bundle 'VimIM'

"Bundle 'minibufexplorerpp'
Bundle 'bufexplorer.zip'

Bundle 'vim-powerline'

Bundle 'sketch.vim'

" non github repos
"Bundle 'git://git.wincent.com/command-t.git'
" git repos on your local machine (ie. when working on your own plugin)
"Bundle 'file:///Users/gmarik/path/to/plugin'
" ...

filetype plugin indent on     " required!
"
" Brief help
" :BundleList          - list configured bundles
" :BundleInstall(!)    - install(update) bundles
" :BundleSearch(!) foo - search(or refresh cache first) for foo
" :BundleClean(!)      - confirm(or auto-approve) removal of unused bundles
"
" see :h vundle for more details or wiki for FAQ
" NOTE: comments after Bundle command are not allowed..

set wildmenu

set tabstop=4
set softtabstop=4
set shiftwidth=4
"set expandtab
set ts=4
set nobackup

set nowrap
syntax on

if has("gui_running")
colorscheme pablo
else
colorscheme elflord
endif

set ruler
set number

" auto maximize the vim window in GUI mode
au GUIEnter * simalt ~x
:set guioptions-=m  "remove menu bar
:set guioptions-=T  "remove toolbar
:set guioptions-=r  "remove right-hand scroll bar

" sketch
map <F2> :call ToggleSketch()<CR>

" MiniBufExplorer
let g:miniBufExplMapWindowNavVim = 1
let g:miniBufExplMapWindowNavArrows = 1
let g:miniBufExplMapCTabSwitchBufs = 1
let g:miniBufExplModSelTarget = 1

" set proxy
let $http_proxy="proxymsn.zte.com.cn:80"
let $https_proxy="proxymsn.zte.com.cn:80"

" powerline
let g:Powerline_symbols = 'fancy'
set laststatus=2
