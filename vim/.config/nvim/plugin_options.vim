let g:UltiSnipsExpandTrigger="<c-j>"
let g:EditorConfig_exclude_patterns = ['fugitive://.*', 'scp://.*']

let g:ale_lint_on_text_changed = 'never'
let g:gist_detect_filetype = 1
let g:gist_browser_command = 'echo %URL% | xclip'

" Enable syntax highlighting in fenced code blocks
let g:markdown_fenced_languages = ['python', 'html', 'javascript', 'css', 'bash=sh', 'sh']


" tree-view
let g:netrw_liststyle = 3

let g:LanguageClient_autoStart = 1
let g:LanguageClient_diagnosticsList = "Location"
let g:LanguageClient_serverCommands = {
    \ 'haskell': ['stack', 'exec', 'hie', '--', '--lsp'],
    \ 'python': ['pyls'],
    \ 'java': [
    \   'java',
    \   '-Declipse.application=org.eclipse.jdt.ls.core.id1',
    \   '-Dosgi.bundles.defaultStartLevel=4',
    \   '-Declipse.product=org.eclipse.jdt.ls.core.product',
    \   '-Dlog.protocol=true',
    \   '-Dlog.level=ALL',
    \   '-noverify',
    \   '-Xmx1G',
    \   '-jar',
    \   glob($HOME . '/workspace/code/eclipse/eclipse.jdt.ls/org.eclipse.jdt.ls.product/target/repository/plugins/org.eclipse.equinox.launcher_*.jar'),
    \   '-configuration',
    \   $HOME . '/workspace/code/eclipse/eclipse.jdt.ls/org.eclipse.jdt.ls.product/target/repository/config_linux',
    \   '-data',
    \   $HOME . '/.local/share/eclipse'
    \ ]
    \ }
