set shortmess+=wI

let mapleader = ","
let maplocalleader = " "

set background=dark
colorscheme gruvbox

set matchtime=3 " show matching parenthesis a bit faster.
set cmdheight=2
set nu rnu numberwidth=4
set mouse=a
set cursorline
set hidden
set lazyredraw
set ttyfast
set scrolloff=3
set termguicolors

set complete=.,w,b,u,U,i,d,t
set completeopt=longest,menuone,noselect
set infercase

set wildmode=longest:full,full
set wildignore+=*.pyc,.git,.idea,*.o
set suffixes+=.pyc,.tmp                     " along with the defaults, ignore these

set path-=/usr/include

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

set diffopt=filler,internal,hiddenoff,algorithm:histogram,indent-heuristic

augroup nvim_opts
    au!
    au FileType markdown,rst,text,mail setlocal keywordprg=:sp\ term://sdcv\ -n\ -c
    au FileType markdown,rst,text,mail setlocal spell
    au FileType markdown,rst,text,mail setlocal complete+=kspell
    au TermOpen * setlocal nonumber norelativenumber signcolumn=no

    " Auto-create parent directories. But not for URIs (paths containing "://").
    au BufWritePre,FileWritePre * if @% !~# '\(://\)' | call mkdir(expand('<afile>:p:h'), 'p') | endif
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

if has('nvim-0.5')
    lua LspConf = require "lsp-config"
    lua LspExt = require "lsp-ext"
    lua LspDiag = require "lsp-diagnostics"
    augroup lsp
      au!
      au Filetype java lua LspConf.start_jdt()
      au Filetype haskell lua LspConf.start_hie()
      au Filetype python lua LspConf.add_client({'pyls'})
      au Filetype html lua LspConf.add_client({'html-languageserver', '--stdio'}, {name='html-ls'})
      au Filetype go lua LspConf.start_go_ls()
      au Filetype sh lua LspConf.add_client({'bash-language-server', 'start'}, {name = 'bash-ls'})
      au Filetype rust lua LspConf.add_client({'rls'}, {root={'Cargo.toml', '.git'}})
      au Filetype lua lua LspConf.add_client({'lua-lsp'})
      au Filetype json lua LspConf.add_client({'json-languageserver', '--stdio'}, {name='json-ls'})
      au InsertCharPre * lua LspExt._InsertCharPre()
      au InsertLeave * lua LspExt._InsertLeave()
      au CursorMoved,CursorMovedI * lua LspDiag.show_diagnostics()
    augroup end
endif
