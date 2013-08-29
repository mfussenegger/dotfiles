" Use vim instead of vi settings.
set nocompatible

if has('vim_starting')
    set runtimepath+=~/.vim/bundle/neobundle.vim
endif

call neobundle#rc(expand('~/.vim/bundle/'))
NeoBundleFetch 'Shougo/neobundle.vim'

" for async commands (NeoBundle, Unite)
NeoBundle 'Shougo/vimproc', {
    \ 'build' : {
    \   'unix' : 'make -f make_unix.mak',
    \   },
    \ }

" unite stuff
NeoBundle 'Shougo/unite.vim'
NeoBundleLazy 'Shougo/unite-outline', {'autoload': {'unite_sources': 'outline'}}
NeoBundleLazy 'osyo-manga/unite-quickfix', {'autoload':{'unite_sources':
            \ ['quickfix', 'location_list']}}
NeoBundle 'mfussenegger/unite-git'

" generic plugins {{{

NeoBundle 'Valloric/YouCompleteMe', {
    \ 'build' : {
    \   'unix' : 'sh install.sh',
    \   },
    \ }
NeoBundle 'mattn/webapi-vim'
NeoBundle 'mattn/gist-vim'
NeoBundle 'vim-scripts/linediff.vim'
NeoBundle 'https://git.gitorious.org/vim-gnupg/vim-gnupg.git'
NeoBundle 'tpope/vim-fugitive'
NeoBundle 'scrooloose/nerdtree'

" }}}

NeoBundle 'SirVer/ultisnips'

" sql
NeoBundle 'vim-scripts/dbext.vim'

" python {{{

" remove this once the features are in YouCompleteMe see issue #119
NeoBundleLazy 'davidhalter/jedi-vim', {'autoload': {'filetypes': ['python']}}
NeoBundleLazy 'hynek/vim-python-pep8-indent', {'autoload': {'filetypes': ['python']}}
NeoBundleLazy 'alfredodeza/khuno.vim', {'autoload': {'filetypes': ['python']}}


" }}}

" salt
NeoBundle 'saltstack/salt-vim'

" Syntax {{{

NeoBundleLazy 'vim-scripts/JSON.vim', {'autoload': {'filetypes': ['json']}}

" }}}

" HTML/CSS {{{

NeoBundleLazy 'othree/html5.vim', {'autoload':
            \ {'filetypes': ['html', 'xhttml', 'css']}}

" }}}


NeoBundleCheck

" powerline
" deprecated and replaced with Lokaltog/powerline
" but using the package from AUR
" NeoBundle 'Lokaltog/vim-powerline', {'rev': 'develop'}


set shortmess+=I                    " Don't show vim welcome screen

syntax on
filetype indent plugin on


let mapleader = ","
let maplocalleader = " "

set history=200
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
    let g:solarized_termcolors=256
    set background=dark
    colorscheme zenburn
endif

if has('win32')
    inoremap <c-v> <esc>"*p<return>i
    vnoremap <c-c> "+y
    set gfn=Andale_Mono:h11:cANSI
endif

set ttyfast

set showmatch
set matchtime=3

set cmdheight=2
set laststatus=2                            " always have a status line
set ruler
set rnu numberwidth=4
set mouse=a
set cursorline
set backspace=2                             " allow backspacing over indent, eol, start

set complete=.,t,i,b,w,k
set completeopt=longest,menuone,preview
set infercase
"
set wildchar=<tab>
set wildmenu
set wildmode=list:longest,full
set wildignore+=*.pyc,.git,.idea
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

set listchars=tab:>.,trail:.,extends:#,nbsp:.

" change working directory automatically
" deactivated - ctrlp works better without
" set autochdir
"
set scrolloff=3

if has("folding")
    set foldenable
    set foldmethod=syntax
    set foldlevel=2
endif

" Resize splits when the window is resized
autocmd VimResized * :wincmd =

" improve search
set ignorecase
set smartcase
set hlsearch
set incsearch
set gdefault

" faster scrolling
nnoremap <C-e> 3<C-e>
nnoremap <C-y> 3<C-y>

set matchpairs+=<:> " pairs for % command

" no backup files
set nobackup
set noswapfile

" indent settings
set smartindent
set smarttab


" template support
autocmd BufNewFile * silent! 0r $HOME/.vim/templates/%:e.tpl

" mappings {{{
noremap <F7> :w<CR>:!./"%"<CR>
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
" khuno
" =============================================================================

" color column is enough for me to try to stick to 79 columns
" so relax with warnings a bit
let g:khuno_max_line_length = 99

" =============================================================================
" ultiSnips
" =============================================================================

" <tab> is already used by YouCompleteMe, so use <c-j> as expand trigger
let g:UltiSnipsExpandTrigger="<c-j>"

" =============================================================================
" unite
" =============================================================================
"

call unite#custom#profile('git/ls-files', 'matchers', ['matcher_fuzzy'])
call unite#custom#profile('git/ls-files', 'filters', ['sorter_rank'])
call unite#custom#source('file_mru,file_rec,file_rec/async,grep,locate',
            \ 'ignore_pattern', join(['\.git/', 'tmp/', 'bundle/'], '\|'))

" general fuzzy search
nnoremap <silent><localleader><space> :Unite
            \ -buffer-name=files -start-insert 
            \ buffer file_mru bookmark file_rec/async<CR>

" search for files recursive
nnoremap <silent><localleader>t :Unite
            \ -buffer-name=files -no-split -start-insert file_rec/async<CR>

nnoremap <silent><localleader>f :Unite
            \ -buffer-name=files -no-split -start-insert git/ls-files<CR>


" quick registers
nnoremap <silent><localleader>r :Unite -buffer-name=register register<CR>

" buffer
nnoremap <silent><localleader>b :Unite
            \ -buffer-name=files -no-split -start-insert buffer<cr>

" quick outline
nnoremap <silent><localleader>o :Unite -silent -direction=topleft -winwidth=40
            \ -buffer-name=outline -start-insert -vertical outline<CR>

" quick switch lcd
nnoremap <silent><localleader>d :Unite -toggle
            \ -buffer-name=change-cwd -default-action=lcd directory_mru<CR>

" tasks
nnoremap <silent><localleader>; :Unite -silent -toggle
            \ grep:.::FIXME\|TODO\|XXX<CR>

" grep
nnoremap <localleader>g :Unite -silent -no-quit grep<CR>


" use ack if available
if executable('ack')
    let g:unite_source_grep_command = 'ack'
    let g:unite_source_grep_default_opts = '--no-group --no-color'
    let g:unite_source_grep_recursive_opt = ''
    let g:unite_source_grep_search_word_highlight = 1
endif

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

nnoremap <F9> :TagbarToggle<CR>
nnoremap <silent> <F10> :NERDTreeToggle<CR>
inoremap <silent> <F10> <esc>:NERDTreeToggle<cr>

" open nerdtree if vim was opened with no files specified
" autocmd vimenter * if !argc() | NERDTree | endif

" close vim if nerdtree is the last open window
" autocmd bufenter * if (winnr("$") == 1 && exists("b:NERDTreeType") && b:NERDTreeType == "primary") | q | endif


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
nnoremap <leader>gg :exe 'silent Ggrep -i '.input("Pattern: ")<Bar>Unite
            \ quickfix -no-quit<CR>
nnoremap <leader>gl :exe "silent Glog <Bar> Unite -no-quit
            \ quickfix"<CR>:redraw!<CR>

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

let g:Powerline_symbols = 'compatible'

let g:gist_detect_filetype = 1

" might need to set g:tagbar_ctags_bin
" Settings for tagbar.vim
let g:tagbar_compact=1
let g:tagbar_width=28


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

    autocmd FileType python noremap <buffer> <F2> :w<CR>:!python -i "%"<CR>
    autocmd FileType python noremap <buffer> <F5> :w<CR>:!python "%"<CR>
    autocmd FileType python noremap <buffer> <F6> :w<CR>:!python -m pdb "%"<CR>
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


" Vimscript file settings {{{
augroup ft_vim
    autocmd!
    autocmd FileType vim setlocal foldmethod=marker
augroup end
" }}}
