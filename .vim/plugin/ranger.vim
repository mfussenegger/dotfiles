" use ranger to open another file

fun! RangerChooser()
   exec "silent !ranger --choosefile=/tmp/chosenfile " . expand("%:p:h")
   if filereadable('/tmp/chosenfile')
     exec 'edit ' . system('cat /tmp/chosenfile')
     call system('rm /tmp/chosenfile')
   endif
   redraw!
endfun

nnoremap <leader>m :call RangerChooser()<CR>
