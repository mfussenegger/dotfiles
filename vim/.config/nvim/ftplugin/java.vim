if filereadable("./gradlew")
    let test#java#runner = 'gradletest'
    let test#java#gradletest#executable = './gradlew test'
    setlocal makeprg=./gradlew\ --no-daemon\ -q
else
    setlocal makeprg=gradle\ --no-daemon\ -q
endif
lua require('ft.java').attach()
