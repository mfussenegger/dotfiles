call pathogen#infect()
syntax on
filetype indent plugin on

let mapleader = ","

set lazyredraw

set hidden

" sane paste
set nopaste

" encoding
set enc=utf-8
set fileencoding=utf-8

" spellcheck
map <F11> :setlocal spell<CR>
set spelllang=en,de

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
    colors zenburn
endif

set cmdheight=2
set laststatus=2
set statusline=%f\ %r%h%w\ [format=%{&ff}]\ [enc=%{&fenc}]\ [type=%Y]\ [bom=%{&bomb}]\ [hex=\%02.2B]\ [%l,%c\ %P%M]
set ruler
set nu
set mouse=a
set cursorline
set listchars=tab:▷⋅,trail:⋅,nbsp:⋅,eol:$   " type :set list to activate
set backspace=2                             " allow backspacing over indent, eol, start

set complete=.,t,i,b,w,k
set completeopt=longest,menu,menuone,preview
set infercase
"
set wildchar=<tab>
set wildmenu
set wildmode=longest:full,full
set suffixes+=.pyc,.tmp                     " along with the defaults, ignore these

" change working directory automatically
set autochdir
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
autocmd FileType css,html,xhtml,xml,htmldjango,htmljinja,eruby,mako setlocal expandtab shiftwidth=2 tabstop=2 softtabstop=2 tw=120
autocmd FileType csharp setlocal tabstop=8 sw=8

" template support
autocmd BufNewFile * silent! 0r $HOME/.vim/templates/%:e.tpl

" automatically give execute permissions
" if file begins with #! and contains '/bin/'
"
function ModeChange()
    if getline(1) =~ "^#!.*/bin/*"
        silent !chmod u+x <afile>
    endif
endfunction
au BufWritePost *.sh call ModeChange()

" mappings
map <F7> :w<CR>:!./"%"<CR>
autocmd FileType python map <F2> :w<CR>:!python -i "%"<CR>
autocmd FileType python map <F5> :w<CR>:!python "%"<CR>
autocmd FileType python map <F6> :w<CR>:!python -m pdb "%"<CR>
autocmd FileType plaintex,latex,tex map <F5> :w<CR> :!pdflatex -shell-escape "%"<CR>
autocmd FileType plaintex,latex,tex map <F6> :w<CR> :!evince %:p:r.pdf<CR>

" switch buffers like tabs
map <S-J> :bp<cr>
map <S-K> :bn<cr>

cmap w!! %!sudo tee > /dev/null %
imap jj <Esc>

" settings for acp
let g:acp_behaviorKeyWordLength = 3

" might need to set g:tagbar_ctags_bin
" Settings for tagbar.vim
let g:tagbar_compact=1
let g:tagbar_width=28

nnoremap <silent> <F9> :TagbarToggle<CR>
nnoremap <silent> <F10> :NERDTree<CR>

" open nerdtree if vim was opened with no files specified
autocmd vimenter * if !argc() | NERDTree | endif

" close vim if nerdtree is the last open window
autocmd bufenter * if (winnr("$") == 1 && exists("b:NERDTreeType") && b:NERDTreeType == "primary") | q | endif


" Visually Select a method / class and execute it by hitting 'Ctrl+h'
" method defined in ftplugin/python.vim
map <C-h> :py evaluate_range()

au FileType python,c,cpp set tabstop=8
au FileType python set shiftwidth=4
au FileType python set expandtab
fu Select_c_style()
    if search('^\t', 'n', 150)
        set shiftwidth=8
        set noexpandtab
    el
        set shiftwidth=4
        set expandtab
    en
endf
au FileType c,cpp call Select_c_style()
au BufRead,BufNewFile Makefile* set noexpandtab

au FileType python,baan,c,cpp set fileformat=unix
au FileType baan set fileencoding=latin1

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


if &term =~ "rxvt"
        "Set the cursor white in cmd-mode and orange in insert mode
        let &t_EI = "\<Esc>]12;white\x9c"
        let &t_SI = "\<Esc>]12;orange\x9c"
        "We normally start in cmd-mode
        silent !echo -e "\e]12;white\x9c"
endif

if has("autocmd") && exists("+omnifunc")
    autocmd FileType *
        \ if &omnifunc == "" |
        \   setlocal omnifunc=syntaxcomplete#Complete |
        \ endif
endif

