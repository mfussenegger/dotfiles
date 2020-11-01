set shortmess+=wI

let mapleader = ","
let maplocalleader = " "

set background=dark
colorscheme gruvbox8_hard

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

set updatetime=500

set complete=.,w,b,u,U,i,d,t
set completeopt=menuone,noinsert,noselect
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
        autocmd InsertEnter * set colorcolumn=80,120
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
    packadd nvim-jdtls
    packadd nvim-dap
    packadd nvim-fzy

    lua require('jdtls').jol_path = os.getenv('HOME') .. '/apps/jol.jar'
    lua require('jdtls.ui').pick_one_async = require('fzy').pick_one
    lua require('dap.ui').pick_one = require('fzy').pick_one
    lua lsp_ext = require('lsp-ext')
    augroup lsp
      au!
      au FileType java lua require('lsp-config').start_jdt()
      au FileType haskell lua require('lsp-config').start_hie()
      au FileType python lua require('lsp-config').add_client({'pyls'})
      au FileType html lua require('lsp-config').add_client({'html-languageserver', '--stdio'}, {name='html-ls'})
      au FileType go lua require('lsp-config').start_go_ls()
      au FileType sh lua require('lsp-config').add_client({'bash-language-server', 'start'}, {name = 'bash-ls'})
      au FileType rust lua require('lsp-config').add_client({'rls'}, {root={'Cargo.toml', '.git'}})
      au FileType lua lua require('lsp-config').start_lua_ls()
      au FileType json lua require('lsp-config').add_client({'json-languageserver', '--stdio'}, {name='json-ls'})
      au FileType css lua require('lsp-config').add_client({'css-languageserver', '--stdio'}, {name='css-ls'})
    augroup end

    lua require('dap-config')
endif


highlight! link LspReferenceText Search
highlight! link LspReferenceRead Search
highlight! link LspReferenceWrite Search
