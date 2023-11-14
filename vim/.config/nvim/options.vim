set shortmess+=wI

let mapleader = ","
let maplocalleader = " "
set termguicolors

set matchtime=3 " show matching parenthesis a bit faster.
set nu rnu numberwidth=4
set cursorline
set scrolloff=3

set updatetime=1000

set complete=.,w,b,u,U,i,d,t
set completeopt=menuone,noinsert,noselect

set wildmode=longest:full,full
set wildignore+=*.pyc,.git,.idea,*.o
if has('nvim-0.9')
  set wildoptions=pum,tagfile,fuzzy
endif
set suffixes+=.pyc,.tmp                     " along with the defaults, ignore these

set path-=/usr/include

" do not select the end of line
set selection=old

set expandtab
set shiftwidth=2
set softtabstop=2
set shiftround

" improve search
set ignorecase
set smartcase

set matchpairs+=<:> " pairs for % command

set noswapfile

" smart autoindenting when starting a new line
set smartindent

set thesaurus=~/.local/share/nvim/mthesaur.txt
set dictionary=/usr/share/dict/words
set spelllang=en,de

let &showbreak = '↪ '
let &listchars = 'tab:▸ ,extends:❯,precedes:❮,nbsp:±,trail:⣿'
set list

set diffopt=filler,internal,hiddenoff,algorithm:histogram,indent-heuristic

augroup nvim_opts
    au!
    au FileType markdown,rst,text,mail source ~/.config/nvim/prose.vim
    au TermOpen * setlocal nonumber norelativenumber signcolumn=no

    " Auto-create parent directories. But not for URIs (paths containing "://").
    au BufWritePre,FileWritePre * if @% !~# '\(://\)' | call mkdir(expand('<afile>:p:h'), 'p') | endif
augroup end

augroup colorcol
  autocmd!
  autocmd InsertEnter * set colorcolumn=80,120
  autocmd InsertLeave * set colorcolumn=""
augroup end

set cpoptions-=a

if executable("rg")
    set grepprg=rg\ --vimgrep\ --no-heading
    set grepformat=%f:%l:%c:%m,%f:%l:%m
endif


function! MyQuickfixtext(opts)
  return luaeval('U.quickfixtext(_A)', a:opts)
endfunction

set statusline=%!v:lua.require'me'.statusline()
set quickfixtextfunc=MyQuickfixtext

hi! def link LspReferenceText IncSearch
hi! def link LspReferenceRead IncSearch
hi! def link LspReferenceWrite IncSearch
hi! def link LspCodeLens Include
hi! def link LspSignatureActiveParameter WarningMsg
hi! def link NormalFloat Normal


sign define DiagnosticSignError text= texthl= linehl= numhl=ErrorMsg
sign define DiagnosticSignWarn text= texthl= linehl= numhl=WarningMsg
sign define DiagnosticSignInfo text= texthl= linehl= numhl=Underlined
sign define DiagnosticSignHint text= texthl= linehl= numhl=Underlined
