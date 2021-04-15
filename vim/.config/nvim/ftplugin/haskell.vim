if executable("ormolu")
    setlocal formatprg=ormolu
elseif executable("stylish-haskell")
    setlocal formatprg=stylish-haskell
endif

setlocal tabstop=2
setlocal shiftwidth=2
setlocal softtabstop=2
setlocal makeprg=stack\ build
setlocal errorformat=%f:%l:%v:%m
setlocal include=^import\\s*\\(qualified\\)\\?\\s*
setlocal includeexpr=substitute(v:fname,'\\.','/','g').'.'
setlocal suffixesadd=.hs

nnoremap <buffer> <silent>gO :Vista!!<CR>
