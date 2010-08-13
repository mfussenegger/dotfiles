syntax on
filetype indent plugin on

" encoding
set enc=utf-8
set fileencoding=utf-8

" spellcheck
map <F11> :setlocal spell spelllang=de<CR>

" visual stuff
set t_Co=256
colors zenburn
if has('gui')
	set guioptions-=m
	set guioptions-=T
	set guioptions-=l
	set guioptions-=L
	set guioptions-=r
	set guioptions-=R
	set gfn=Terminus\ 12
endif

set cmdheight=2
set laststatus=2
set statusline=[%l,%c\ %P%M]\ %f\ %r%h%w

set complete=.,t,i,b,w,k
set completeopt=longest,menuone
"set completeopt-=preview
"let g:pydiction_location = '$HOME/.vim/ftplugin/pydiction/complete-dict'
"
set wildchar=<tab>
set wildmenu
set wildmode=longest:full,full
set wildignore+=*~,*.o,*.tmp
set ruler
set nu
set mouse=a

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
autocmd FileType python,c setlocal textwidth=79
autocmd FileType css,html,xhtml,xml,htmldjango,htmljinja,eruby,mako setlocal expandtab shiftwidth=2 tabstop=2 softtabstop=2

" template support
autocmd BufNewFile * silent! 0r $HOME/.vim/templates/%:e.tpl

" mappings
autocmd FileType python map <F5> :w<CR>:!python "%"<CR>
autocmd FileType python map <F6> :w<CR>:!python -m pdb "%"<CR>
autocmd FileType plaintex,latex,tex map <F5> :w<CR> :!pdflatex "%"<CR>
autocmd FileType plaintex,latex,tex map <F6> :w<CR> :!evince %:p:r.pdf<CR>


cmap w!! %!sudo tee > /dev/null %
imap jj <Esc>

nnoremap <silent> <F9> :Tlist<CR>
nnoremap <silent> <F10> :NERDTree<CR>

" Visually Select a method / class and execute it by hitting 'Ctrl+h'

python << EOL
import vim
def EvaluateCurrentRange():
	eval(compile('\n'.join(vim.current.range),'','exec'),globals())
EOL
map <C-h> :py EvaluateCurrentRange()

" type :make and get a list of syntax errors: 
" You will have the ability to to type :cn and :cp to move around the error list. You can also type :clist to see all the errors
autocmd BufRead *.py set makeprg=python\ -c\ \"import\ py_compile,sys;\ sys.stderr=sys.stdout;\ py_compile.compile(r'%')\"
autocmd BufRead *.py set efm=%C\ %.%#,%A\ \ File\ \"%f\"\\,\ line\ %l%.%#,%Z%[%^\ ]%\\@=%m
" Hightlight chars that go over the 80 column limit
autocmd FileType python highlight OverLength ctermbg=red ctermfg=white guibg=red guifg=white
autocmd FileType python match OverLength '\%81v.*'



" Settings for taglist.vim
let Tlist_Use_Right_Window=0
let Tlist_Auto_Open=0
let Tlist_Enable_Fold_Column=0
let Tlist_Compact_Format=1
let Tlist_WinWidth=28
let Tlist_Exit_OnlyWindow=1
let Tlist_File_Fold_Auto_Close = 1


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

if &term =~ "rxvt"
        "Set the cursor white in cmd-mode and orange in insert mode
        let &t_EI = "\<Esc>]12;white\x9c"
        let &t_SI = "\<Esc>]12;orange\x9c"
        "We normally start in cmd-mode
        silent !echo -e "\e]12;white\x9c"
endif
