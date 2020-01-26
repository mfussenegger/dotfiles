let g:echodoc#enable_at_startup = 1
let g:echodoc#type = "floating"
let g:UltiSnipsExpandTrigger="<c-j>"
let g:EditorConfig_exclude_patterns = ['fugitive://.*', 'scp://.*']

let g:ale_lint_on_text_changed = 'never'
let g:gist_detect_filetype = 1
let g:gist_browser_command = 'echo %URL% | xclip'

" Enable syntax highlighting in fenced code blocks
let g:markdown_fenced_languages = ['python', 'html', 'javascript', 'css', 'bash=sh', 'sh']

let test#strategy = "neovim"

" See vim-gutentags issue # 168
augroup gutentags
  autocmd!
  autocmd FileType gitcommit,gitrebase let g:gutentags_enabled=0
  autocmd BufEnter .git/PULLREQ_EDITMSG let g:gutentags_enabled=0
augroup end


if executable('hasktags')
  let g:vista_ctags_cmd = {
      \ 'haskell': 'hasktags -x -o - -c',
      \ }
endif

let g:vista_executive_for = {
  \ 'haskell': 'nvim_lsp',
  \ 'python': 'nvim_lsp'
  \ }


" tree-view
let g:netrw_liststyle = 3
