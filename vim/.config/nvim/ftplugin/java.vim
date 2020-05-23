if filereadable("./gradlew")
    let test#java#runner = 'gradletest'
    let test#java#gradletest#executable = './gradlew --parallel test'
    setlocal makeprg=./gradlew\ --no-daemon\ -q
else
    setlocal makeprg=gradle\ --no-daemon\ -q
endif

if has('nvim-0.5')
  lua require('ft.java').attach()
endif
