set statusline=%<»\ %f\ %h%m%r%=

set statusline+=%#warningmsg#
set statusline+=%{&paste?'[paste]\ ':''}
set statusline+=%*

set statusline+=%#warningmsg#
set statusline+=%{&ff!='unix'?'['.&ff.']\ ':''}
set statusline+=%*

set statusline+=%#warningmsg#
set statusline+=%{(&fenc!='utf-8'&&&fenc!='')?'['.&fenc.']\ ':''}
set statusline+=%*

set statusline+=%-14.(%l,%c%)
