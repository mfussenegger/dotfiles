set hidden
nnoremap <buffer> <silent> F5 :call LanguageClient_contextMenu()<CR>
nnoremap <buffer> <silent> K :call LanguageClient_textDocument_hover()<CR>
nnoremap <buffer> <silent> gd :call LanguageClient_textDocument_definition()<CR>
nnoremap <buffer> <silent> gr :call LanguageClient_textDocument_references()<CR>
nnoremap <buffer> <silent> <leader>fs :call LanguageClient_textDocument_documentSymbol()<CR>
nnoremap <buffer> <silent> <leader>fS :call LanguageClient_workspace_symbol()<CR>
nnoremap <buffer> <silent> crr :call LanguageClient_textDocument_rename()<CR>
nnoremap <buffer> <silent> <a-CR> :call LanguageClient_textDocument_codeAction()<CR>
ALEDisableBuffer
