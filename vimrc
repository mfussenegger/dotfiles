filetype off
call pathogen#infect()

" Use vim instead of vi settings.
set nocompatible

set shortmess+=I                    " Don't show vim welcome screen

syntax on
filetype indent plugin on

let mapleader = ","
let maplocalleader = ";"

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
    colorscheme solarized
    set background=dark
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
set wildmode=list:longest:full,full
set wildignore+=*.pyc,.git
set suffixes+=.pyc,.tmp                     " along with the defaults, ignore these

set tabstop=4
set shiftwidth=4
set softtabstop=4
set expandtab
set shiftround
set wrap

set listchars=tab:>.,trail:.,extends:#,nbsp:.

" change working directory automatically
" deactivated - command-T works better without
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

" set working directory
noremap <leader>. :lcd %:p:h<CR>

" uppercase word in insert mode
" inoremap <c-u> <esc>viwUea

nnoremap <leader>ev :split $MYVIMRC<cr>
nnoremap <leader>sv :source $MYVIMRC<cr>

nnoremap <leader>s :%s//<left>

" highlight trailing whitespaces
nnoremap <leader>w :match Error /\v +$/<cr>
nnoremap <leader>W :match none<cr>


" buffer navigation
noremap <c-j> :bp<cr>
noremap <c-k> :bn<cr>

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

" gnupg
nnoremap <leader>pe :GPGEditRecipients<cr>
nnoremap <leader>pv :GPGViewRecipients<cr>

" fugitive {{{
"
nnoremap <leader>gw :Gwrite<cr>
nnoremap <leader>gd :Gdiff<cr>
nnoremap <leader>gs :Gstatus<cr>
nnoremap <leader>gb :Gblame<cr>
nnoremap <leader>gci :Gcommit<cr>

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

" settings for acp
let g:acp_behaviorKeyWordLength = 3
let g:acp_behaviorHtmlOmniLength = 2


" might need to set g:tagbar_ctags_bin
" Settings for tagbar.vim
let g:tagbar_compact=1
let g:tagbar_width=28

let g:pymode_rope_guess_project = 0

" open nerdtree if vim was opened with no files specified
" autocmd vimenter * if !argc() | NERDTree | endif

" close vim if nerdtree is the last open window
autocmd bufenter * if (winnr("$") == 1 && exists("b:NERDTreeType") && b:NERDTreeType == "primary") | q | endif

if exists("&colorcolumn")
    autocmd InsertEnter * set colorcolumn=80
    autocmd InsertLeave * set colorcolumn=""
endif

autocmd InsertEnter * :set nu
autocmd InsertLeave * :set rnu

" }}}


" tab label {{{
" improved tab display shameless stolen from http://blog.golden-ratio.net/2008/08/19/using-tabs-in-vim/
function! GuiTabLabel()
    " add the tab number
    let label = '['.tabpagenr()

    " modified since the last save?
    let buflist = tabpagebuflist(v:lnum)
    for bufnr in buflist
        if getbufvar(bufnr, '&modified')
            let label .= '*'
            break
        endif
    endfor

    " count number of open windows in the tab
    let wincount = tabpagewinnr(v:lnum, '$')
    if wincount > 1
        let label .= ', '.wincount
    endif
    let label .= '] '

    " add the file name without path information
    let n = bufname(buflist[tabpagewinnr(v:lnum) - 1])
    let label .= fnamemodify(n, ':t')

    return label
endfunction

set guitablabel=%{GuiTabLabel()}
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
    " Visually Select a method / class and execute it by hitting 'Ctrl+h'
    " method defined in ftplugin/python.vim
    autocmd FileType python noremap <buffer> <C-i> :py evaluate_range()

    autocmd FileType python setlocal fileformat=unix

    autocmd FileType python noremap <buffer> <F2> :w<CR>:!python -i "%"<CR>
    autocmd FileType python noremap <buffer> <F5> :w<CR>:!python "%"<CR>
    autocmd FileType python noremap <buffer> <F6> :w<CR>:!python -m pdb "%"<CR>
    autocmd BufWritePost *.py call Flake8()
augroup end
" }}}
"

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
