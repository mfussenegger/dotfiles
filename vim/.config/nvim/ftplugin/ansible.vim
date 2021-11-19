
setlocal keywordprg=:sp\ term://ansible-doc
nnoremap <buffer><silent><leader>te :w<CR> :lua require('term').run_ansible()<CR>
