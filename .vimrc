call pathogen#infect()

syntax on
filetype indent plugin on

let mapleader = ","

set lazyredraw
set hidden
" sane paste
set nopaste
set enc=utf-8
set fileencoding=utf-8

" visual stuff
set t_Co=256
if has('gui_running')
    colors wombat
    set guioptions-=m
    set guioptions-=T
    set guioptions-=l
    set guioptions-=L
    set guioptions-=r
    set guioptions-=R
    set gfn=Terminus\ 12
else
    let g:zenburn_high_Contrast = 1
    colors zenburn
endif


set cmdheight=2
set laststatus=2                            " always have a status line
set statusline=%.20F                        " filename
set statusline+=\ [format=%{&ff}]           " fileformat
set statusline+=\ [enc=%{&fenc}]            " encoding
set statusline+=\ [ft=%Y]                   " detected FileType
set statusline+=\ [a=%03b\ 0x%02.2B]        " ascii & hex
set statusline+=%=                          " switch to right side of statusline
set statusline+=\ %m%r%h%w                  " modified flag, readonly flag, helpbuffer flag, preview window flag
set statusline+=[l=%04l\ c=%02c\ %P]      " current line and column number
set ruler
set number numberwidth=4
set mouse=a
set cursorline
set listchars=tab:¿¿,trail:¿,nbsp:¿,eol:$   " type :set list to activate
set backspace=2                             " allow backspacing over indent, eol, start

set complete=.,t,i,b,w,k
set completeopt=longest,menu,menuone,preview
set infercase
"
set wildchar=<tab>
set wildmenu
set wildmode=longest:full,full
set suffixes+=.pyc,.tmp                     " along with the defaults, ignore these

set textwidth=80

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

" improve search
set ignorecase
set smartcase
set hlsearch
set incsearch

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

set expandtab shiftwidth=4 tabstop=4 softtabstop=4

" template support
autocmd BufNewFile * silent! 0r $HOME/.vim/templates/%:e.tpl


" latex
let g:tex_flavor = "latex"

" mappings {{{
noremap <F7> :w<CR>:!./"%"<CR>

" uppercase word in insert mode
inoremap <c-u> <esc>viwUea

nnoremap <leader>ev :split $MYVIMRC<cr>
nnoremap <leader>sv :source $MYVIMRC<cr>

" highlight trailing whitespaces
nnoremap <leader>w :match Error /\v +$/<cr>
nnoremap <leader>W :match none<cr>

" use very magic regex on searches by default, see :help magic
nnoremap / /\v

" switch buffers like tabs
noremap <A-j> :bp<cr>
noremap <A-k> :bn<cr>

cnoremap w!! %!sudo tee > /dev/null %
inoremap jj <Esc>

" spellcheck
noremap <F11> :setlocal spell!<CR>
set spelllang=en,de


nnoremap <silent> <F9> :TagbarToggle<CR>
nnoremap <silent> <F10> :NERDTree<CR>
" }}}

" plugin settings {{{

" settings for acp
let g:acp_behaviorKeyWordLength = 3


" don't set acp_behavior more than once.
" it will freak out vim
"
if exists("g:acp_behavior_set") ==# 0
    let g:acp_behavior_set = 1
    let g:acp_behavior = {
                \ 'baan' : [],
                \ }

    " not sure if meetsForKeyword makes sense, but it seems to work
    call add(g:acp_behavior['baan'], {
                \   'command'      : "\<C-x>\<C-o>",
                \   'completefunc' : 'baancomplete#Complete',
                \   'meets'        : 'acp#meetsForKeyword',
                \   'repeat'       : 0,
                \ })
    call add(g:acp_behavior['baan'], {
                \   'command' : "\<C-x>\<C-n>",
                \   'meets'   : 'acp#meetsForKeyword',
                \   'repeat'  : 0,
                \ })
endif

" might need to set g:tagbar_ctags_bin
" Settings for tagbar.vim
let g:tagbar_compact=1
let g:tagbar_width=28

" open nerdtree if vim was opened with no files specified
autocmd vimenter * if !argc() | NERDTree | endif

" close vim if nerdtree is the last open window
autocmd bufenter * if (winnr("$") == 1 && exists("b:NERDTreeType") && b:NERDTreeType == "primary") | q | endif

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



augroup webdev
    autocmd!
    autocmd FileType css,html,xhtml,xml,htmldjango setlocal expandtab shiftwidth=2 tabstop=2 softtabstop=2 tw=120
    autocmd FileType htmljinja,eruby,mako setlocal expandtab shiftwidth=2 tabstop=2 softtabstop=2 tw=120
augroup end


augroup dotNet
    autocmd!
    autocmd FileType csharp,vb setlocal tabstop=8 sw=8
augroup end


autocmd BufRead,BufNewFile Makefile* set noexpandtab

" baan file settings {{{
augroup baan
    autocmd!
    autocmd FileType baan setlocal omnifunc=baancomplete#Complete
    autocmd FileType baan setlocal fileencoding=latin1
    autocmd FileType baan setlocal fileformat=unix
augroup end
" }}}

" latex file settings {{{
augroup tex
    autocmd!
    autocmd FileType tex noremap <buffer> <F5> :w<CR> :!pdflatex -shell-escape "%"<CR>
    autocmd FileType tex noremap <buffer> <F6> :w<CR> :!evince %:p:r.pdf<CR>
augroup end
" }}}

" python file settings {{{
augroup python
    autocmd!
    " Visually Select a method / class and execute it by hitting 'Ctrl+h'
    " method defined in ftplugin/python.vim
    autocmd FileType python noremap <buffer> <C-h> :py evaluate_range()


    autocmd FileType python setlocal tabstop=4
    autocmd FileType python setlocal shiftwidth=4
    autocmd FileType python setlocal expandtab
    autocmd FileType python setlocal fileformat=unix

    autocmd FileType python noremap <buffer> <F2> :w<CR>:!python -i "%"<CR>
    autocmd FileType python noremap <buffer> <F5> :w<CR>:!python "%"<CR>
    autocmd FileType python noremap <buffer> <F6> :w<CR>:!python -m pdb "%"<CR>
augroup end
" }}}

" Vimscript file settings {{{
augroup vim
    autocmd!
    autocmd FileType vim setlocal foldmethod=marker
augroup end
" }}}
