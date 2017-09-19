let g:python3_host_prog = $HOME . '/.virtualenvs/nvim/bin/python'

set shortmess+=I                    " Don't show vim welcome screen

let mapleader = ","
let maplocalleader = " "

set lazyredraw
set ttyfast

set background=dark
colorscheme zenburn

set matchtime=3 " show matching parenthesis a bit faster.
set cmdheight=2
set nu rnu numberwidth=4
set mouse=a
set cursorline

set completeopt=longest,menuone,preview
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

set tabstop=4
set shiftwidth=4
set softtabstop=4
set expandtab
set shiftround
set nowrap

set scrolloff=3

if has("folding")
    set foldenable
    set foldmethod=indent
    set foldlevel=2
endif

" Resize splits when the window is resized
autocmd VimResized * :wincmd =

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

" template support
autocmd BufNewFile * silent! 0r $HOME/.config/nvim/templates/%:e.tpl

" mappings

" close all split windows except the one that is currently active
noremap <leader>o :only<CR>

nnoremap <leader>ev :split $MYVIMRC<cr>
nnoremap <leader>sv :source $MYVIMRC<cr>

" pre-filter history navigation in command-mode with already typed input
cnoremap <c-n> <down>
cnoremap <c-p> <up>

if executable("rg")
    set grepprg=rg\ --vimgrep\ --no-heading
    set grepformat=%f:%l:%c:%m,%f:%l:%m
endif

let g:UltiSnipsExpandTrigger="<c-j>"

" =============================================================================
" fzf
" =============================================================================
"
nnoremap <silent><leader>f/ :History/<CR>
nnoremap <silent><leader>f: :History:<CR>
nnoremap <silent><leader>ff :Files<CR>
nnoremap <silent><leader>fb :Buffers<CR>
nnoremap <silent><leader>ft :BTags<CR>
nnoremap <silent><leader>fg :GitFiles<CR>
nnoremap <silent><leader>gl :BCommits<CR>

" Fuzzy insert mode completion for lines
imap <c-x><c-l> <plug>(fzf-complete-line)

" =============================================================================


" split navigation
noremap <c-h> <c-w>h
noremap <c-j> <c-w>j
noremap <c-k> <c-w>k
noremap <c-l> <c-w>l

" :terminal stuff

tnoremap <c-h> <C-\><C-n><C-w>h
tnoremap <c-j> <C-\><C-n><C-w>j
tnoremap <c-k> <C-\><C-n><C-w>k
tnoremap <c-l> <C-\><C-n><C-w>l

" tn -> terminal new
" te -> terminal execute
" ts -> terminal send
nnoremap <leader>tn :below new term://
nnoremap <silent><leader>te :w<CR> :below new term://%:p<CR>
nnoremap <silent><leader>ts yy<c-w>wp<c-w>pgv
vnoremap <silent><leader>ts y<c-w>wp<c-w>pgv

set spelllang=en,de
autocmd FileType markdown,rst,text setlocal spell!

" gnupg
nnoremap <leader>pe :GPGEditRecipients<cr>
nnoremap <leader>pv :GPGViewRecipients<cr>

" fugitive {{{
"
nnoremap <leader>gw :Gwrite<cr>
nnoremap <leader>gd :Gdiff<cr>
nnoremap <leader>gs :Gstatus<cr>
nnoremap <leader>gb :Gblame<cr>
nnoremap <leader>gc :Gcommit<cr>

" }}}

cnoremap <C-a> <Home>
cnoremap <C-e> <End>


let g:ale_lint_on_text_changed = 'never'
let g:gist_detect_filetype = 1
let g:gist_browser_command = 'echo %URL% | xclip'

" Enable syntax highlighting in fenced code blocks
let g:markdown_fenced_languages = ['python', 'html', 'javascript', 'css', 'bash=sh', 'sh']


if exists("&colorcolumn")
    autocmd InsertEnter * set colorcolumn=80
    autocmd InsertLeave * set colorcolumn=""
endif
