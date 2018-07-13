set shortmess+=wI

let mapleader = ","
let maplocalleader = " "

set background=dark
colorscheme zenburn

set matchtime=3 " show matching parenthesis a bit faster.
set cmdheight=2
set nu rnu numberwidth=4
set mouse=a
set cursorline
set hidden
set lazyredraw
set ttyfast
set scrolloff=3

set completeopt=noinsert,menuone,noselect
set infercase

set wildmode=list:longest,list:full
set wildignore+=*.pyc,.git,.idea,*.o
set suffixes+=.pyc,.tmp                     " along with the defaults, ignore these

" do not select the end of line
set selection=old
" lower the delay of escaping out of other modes
set timeout timeoutlen=1000 ttimeoutlen=0

" live preview for substitute
set inccommand=split

set expandtab
set shiftwidth=2
set softtabstop=2
set shiftround

if has("folding")
    set foldenable
    set foldmethod=indent
    set foldlevel=2
endif

" improve search
set ignorecase
set smartcase
set hlsearch

set matchpairs+=<:> " pairs for % command

" no backup files
set nobackup
set noswapfile

" smart autoindenting when starting a new line
set smartindent

set thesaurus=~/.local/share/nvim/mthesaur.txt
set dictionary=/usr/share/dict/words
set spelllang=en,de

augroup prose
    autocmd!
    autocmd FileType markdown,rst,text,mail setlocal keywordprg=:sp\ term://sdcv\ -n\ -c
    autocmd FileType markdown,rst,text,mail setlocal spell
    autocmd FileType markdown,rst,text,mail setlocal complete+=kspell
augroup end

if exists("&colorcolumn")
    augroup colorcol
        autocmd!
        autocmd InsertEnter * set colorcolumn=80
        autocmd InsertLeave * set colorcolumn=""
    augroup end
endif

" template support
set cpoptions-=a
autocmd BufNewFile * silent! 0r $HOME/.config/nvim/templates/%:e.tpl

if executable("rg")
    set grepprg=rg\ --vimgrep\ --no-heading
    set grepformat=%f:%l:%c:%m,%f:%l:%m
endif
