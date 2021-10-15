set shortmess+=wI

let mapleader = ","
let maplocalleader = " "
set termguicolors

let g:gruvbox_material_background = 'hard'
let g:gruvbox_material_better_performance = 1
set background=dark
colorscheme gruvbox-material

set matchtime=3 " show matching parenthesis a bit faster.
set cmdheight=2
set nu rnu numberwidth=4
set mouse=a
set cursorline
set hidden
set lazyredraw
set ttyfast
set scrolloff=3

set updatetime=1000

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


function! MyQuickfixtext(opts)
  return luaeval('U.quickfixtext(_A)', a:opts)
endfunction

lua require('me').setup()
set statusline=%!v:lua.U.statusline()
set quickfixtextfunc=MyQuickfixtext
augroup lsp
  au!
  au FileType java lua require('me.lsp.conf').start_jdt()
  au FileType haskell lua require('me.lsp.conf').start_hie()
  au FileType python lua require('me.lsp.conf').add_client({'pylsp'})
  au FileType html lua require('me.lsp.conf').add_client({'html-languageserver', '--stdio'}, {name='html-ls'})
  au FileType go lua require('me.lsp.conf').gopls()
  au FileType sh lua require('me.lsp.conf').add_client({'bash-language-server', 'start'}, {name = 'bash-ls'})
  au FileType rust lua require('me.lsp.conf').add_client({'rls'}, {root={'Cargo.toml', '.git'}})
  au FileType lua lua require('me.lsp.conf').start_lua_ls()
  au FileType json lua require('me.lsp.conf').add_client({'json-languageserver', '--stdio'}, {name='json-ls'})
  au FileType css lua require('me.lsp.conf').add_client({'css-languageserver', '--stdio'}, {name='css-ls'})
  au FileType cs lua require('me.lsp.conf').start_omnisharp()
  au FileType c lua require('me.lsp.conf').add_client({'clangd'})
  au FileType tex lua require('me.lsp.conf').add_client({'texlab'})
  au FileType yaml lua require('me.lsp.conf').start_yaml_ls()
  au FileType * lua require('me').init_hl()
  au FileType * lua require('me').enable_lint()

  au User LspDiagnosticsChanged redrawstatus!
  au User LspMessageUpdate redrawstatus!
augroup end

hi! def link LspReferenceText IncSearch
hi! def link LspReferenceRead IncSearch
hi! def link LspReferenceWrite IncSearch
hi! def link LspCodeLens Include
hi! def link LspSignatureActiveParameter WarningMsg
hi! def link NormalFloat Normal

set signcolumn=auto

" support both, nvim-0.5 and latest master
sign define DiagnosticSignError text= texthl= linehl= numhl=ErrorMsg
sign define DiagnosticSignWarn text= texthl= linehl= numhl=WarningMsg
sign define DiagnosticSignInfo text= texthl= linehl= numhl=Underlined
sign define DiagnosticSignHint text= texthl= linehl= numhl=Underlined
sign define LspDiagnosticsSignError text= texthl= linehl= numhl=ErrorMsg
sign define LspDiagnosticsSignWarning text= texthl= linehl= numhl=WarningMsg
sign define LspDiagnosticsSignInformation text= texthl= linehl= numhl=Underlined
sign define LspDiagnosticsSignHint text= texthl= linehl= numhl=Underlined
