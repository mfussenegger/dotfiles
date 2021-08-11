let g:vsnip_snippet_dir = expand('~/.config/vsnip/')
let g:EditorConfig_exclude_patterns = ['fugitive://.*', 'scp://.*']

let g:gist_detect_filetype = 1
let g:gist_browser_command = 'echo %URL% | xclip'

" Enable syntax highlighting in fenced code blocks
let g:markdown_fenced_languages = ['python', 'html', 'javascript', 'css', 'bash=sh', 'sh']

let test#strategy = "neovim"

if executable('hasktags')
  let g:vista_ctags_cmd = {
      \ 'haskell': 'hasktags -x -o - -c',
      \ }
endif

let g:vista_executive_for = {
  \ 'haskell': 'nvim_lsp',
  \ 'python': 'nvim_lsp',
  \ 'java': 'nvim_lsp',
  \ 'yaml': 'nvim_lsp',
  \ 'yaml.ansible': 'nvim_lsp',
  \ }

" tree-view
let g:netrw_liststyle = 3
