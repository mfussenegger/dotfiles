" Use vim instead of vi settings.
set nocompatible

execute pathogen#infect()

set shortmess+=I                    " Don't show vim welcome screen

let mapleader = ","
let maplocalleader = " "

set lazyredraw
set hidden
set nopaste
set encoding=utf-8
set fileencoding=utf-8

" visual stuff
set t_Co=256
if has('gui_running')
    set background=dark
    colorscheme zenburn
    set guioptions-=m
    set guioptions-=T
    set guioptions-=l
    set guioptions-=L
    set guioptions-=r
    set guioptions-=R
    set gfn=Terminus\ 12
else
    set background=dark
    colorscheme zenburn
endif

if has('win32')
    inoremap <c-v> <esc>"*p<return>i
    vnoremap <c-c> "+y
    set gfn=Andale_Mono:h11:cANSI
endif

set ttyfast

" show matching parenthesis a bit faster.
set matchtime=3

set cmdheight=2
set rnu numberwidth=4
set mouse=a
set cursorline

set completeopt=longest,menuone,preview
set infercase
"
set wildchar=<tab>
set wildmode=list:longest,full
set wildignore+=*.pyc,.git,.idea,*.o
set suffixes+=.pyc,.tmp                     " along with the defaults, ignore these

" do not select the end of line
set selection=old

" lower the delay of escaping out of other modes
set timeout timeoutlen=1000 ttimeoutlen=0


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

" faster scrolling
nnoremap <C-e> 3<C-e>
nnoremap <C-y> 3<C-y>

set matchpairs+=<:> " pairs for % command

" no backup files
set nobackup
set noswapfile

" indent settings
set smartindent

" template support
autocmd BufNewFile * silent! 0r $HOME/.vim/templates/%:e.tpl

" mappings {{{
set pastetoggle=<F8>

" re-indent current line
nnoremap <leader>f I <ESC>k$Ja<backspace><CR><ESC>

" set working directory
noremap <leader>. :lcd %:p:h<CR>

" close all split windows except the one that is currently active
noremap <leader>o :only<CR>



" uppercase word in insert mode
" inoremap <c-u> <esc>viwUea

nnoremap <leader>ev :split $MYVIMRC<cr>
nnoremap <leader>sv :source $MYVIMRC<cr>

nnoremap <leader>s :%s//<left>

" highlight trailing whitespaces
nnoremap <leader>w :match Error /\v +$/<cr>
nnoremap <leader>W :match none<cr>


" =============================================================================
" ultiSnips
" =============================================================================

" <tab> is already used by YouCompleteMe, so use <c-j> as expand trigger
let g:UltiSnipsExpandTrigger="<c-j>"

" =============================================================================
" fzf
" =============================================================================
"
nnoremap <silent><localleader><space> :History<CR>
nnoremap <silent><localleader>t :Files<CR>
nnoremap <silent><localleader>b :Buffers<CR>
nnoremap <silent><localleader>o :BTags<CR>
nnoremap <silent><leader>gl :BCommits<CR>

" fzf + git ls-files
nnoremap <silent><localleader>f :call fzf#run({
            \'source': 'git ls-files',
            \'sink': 'e' })<CR>

" =============================================================================

cnoremap w!! %!sudo tee > /dev/null %
inoremap jj <Esc>

" split navigation
noremap <c-h> <c-w>h
noremap <c-j> <c-w>j
noremap <c-k> <c-w>k
noremap <c-l> <c-w>l

" spellcheck
noremap <F11> :setlocal spell!<CR>
set spelllang=en,de

nnoremap <silent> <F10> :NERDTreeToggle<CR>
inoremap <silent> <F10> <esc>:NERDTreeToggle<cr>

nnoremap <F5> :GundoToggle<CR>

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

" }}}

" plugin settings {{{
"
let g:jedi#use_tabs_not_buffers = 0
let g:jedi#popup_on_dot = 0

let g:tex_flavor = 'latex'
let g:tex_viewer = {'app': 'zathura', 'target': 'pdf'}

let g:gist_detect_filetype = 1

let g:syntastic_python_checkers=['frosted']

let NERDTreeIgnore = ['^develop-eggs$', '\.egg-info$']


if exists("&colorcolumn")
    autocmd InsertEnter * set colorcolumn=80
    autocmd InsertLeave * set colorcolumn=""
endif

autocmd InsertEnter * :set nu
autocmd InsertLeave * :set rnu

" }}}


" diff modified / current file {{{
" thanks reddit/r/vim for this gem
" shows the diff between current modified file and the original file on disk
nnoremap <Leader>df :call DiffOrig()<CR>

function! DiffOrig()
    if !exists("b:diff_active") && &buftype == "nofile"
        echoerr "E: Cannot diff a scratch buffer"
        return -1
    elseif expand("%") == ""
        echoerr "E: Buffer doesn't exist on disk"
        return -1
    endif

    if !exists("b:diff_active") || b:diff_active == 0
        let b:diff_active = 1
        let l:orig_filetype = &l:filetype

        leftabove vnew
        let t:diff_buffer = bufnr("%")
        set buftype=nofile

        read #
        0delete_
        let &l:filetype = l:orig_filetype

        diffthis
        wincmd p
        diffthis
    else
        diffoff
        execute "bdelete " . t:diff_buffer
        let b:diff_active = 0
    endif
endfunction
" }}}


" white cursor in cmd-mode, orange in insert mode {{{
if &term =~ "rxvt"
    "Set the cursor white in cmd-mode and orange in insert mode
    let &t_EI = "\<Esc>]12;white\x9c"
    let &t_SI = "\<Esc>]12;orange\x9c"
    "We normally start in cmd-mode
    silent !echo -e "\e]12;white\x9c"
endif
" }}}

if has("autocmd") && exists("+omnifunc")
    autocmd FileType *
                \ if &omnifunc == "" |
                \   setlocal omnifunc=syntaxcomplete#Complete |
                \ endif
endif


function! Tags()
    silent !git ls-files | xargs ctags
endfunction
command! Tags call Tags()

" ----- file type settings
"
" automatically give execute permissions
" if file begins with #! and contains '/bin/'
"
function! ModeChange()
    if getline(1) =~ "^#!.*/bin/*"
        silent !chmod u+x <afile>
    endif
endfunction
autocmd BufWritePost *.sh call ModeChange()

augroup ft_webdev
    autocmd!
    autocmd FileType css,html,xhtml,xml,htmldjango setlocal expandtab shiftwidth=2 tabstop=2 softtabstop=2 tw=120
    autocmd FileType htmljinja,eruby,mako setlocal expandtab shiftwidth=2 tabstop=2 softtabstop=2 tw=120
augroup end

augroup ft_puppet
    autocmd!
    autocmd FileType puppet setlocal expandtab shiftwidth=2 tabstop=2 softtabstop=2
augroup end

augroup ft_yaml
    autocmd!
    autocmd FileType yaml setlocal expandtab shiftwidth=2 tabstop=2 softtabstop=2
augroup end

augroup ft_dotNet
    autocmd!
    autocmd FileType csharp,vb setlocal tabstop=8 sw=8
augroup end


autocmd BufRead,BufNewFile Makefile* set noexpandtab

" baan file settings {{{
augroup ft_baan
    autocmd!
    autocmd FileType baan setlocal omnifunc=baancomplete#Complete
    autocmd FileType baan setlocal fileencoding=latin1
    autocmd FileType baan setlocal fileformat=unix
augroup end
" }}}

" latex file settings {{{
augroup ft_tex
    autocmd!
    autocmd FileType tex noremap <buffer> <F5> :w<CR> :!pdflatex -shell-escape "%"<CR>
    autocmd FileType tex noremap <buffer> <F6> :w<CR> :!zathura %:p:r.pdf<CR>
augroup end
" }}}

" python file settings {{{
augroup ft_python
    autocmd!
    autocmd FileType python setlocal fileformat=unix
augroup end
" }}}
"

" mutt mail settings {{{
augroup ft_mail
   au BufRead,BufNewFile .followup,.article,.letter,/tmp/pico*,nn.*,snd.*,/tmp/mutt* :set ft=mail
augroup end


" notmuch settings {{{
augroup ft_notmuch
    autocmd!

    autocmd FileType notmuch-folder setlocal foldmethod=manual
    autocmd FileType notmuch-show setlocal foldmethod=manual
augroup end
" }}}


" git file settings {{{
augroup ft_git
    au FileType gitcommit setlocal textwidth=60
augroup end

" }}}

" antlr file settings {{{
augroup ft_antlr
    au BufRead,BufNewFile *.g setlocal ft=antlr3
augroup end

" }}}

" Vimscript file settings {{{
augroup ft_vim
    autocmd!
    autocmd FileType vim setlocal foldmethod=marker
augroup end
" }}}
