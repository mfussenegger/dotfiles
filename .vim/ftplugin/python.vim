let python_highlight_all=1
let python_highlight_indent_errors=1

"
" type :make and get a list of syntax errors:
" You will have the ability to to type :cn and :cp to move around the error
" list. You can also type :clist to see all the errors

set makeprg=python\ -c\ \"import\ py_compile,sys;\ sys.stderr=sys.stdout;\ py_compile.compile(r'%')\"
set efm=%C\ %.%#,%A\ \ File\ \"%f\"\\,\ line\ %l%.%#,%Z%[%^\ ]%\\@=%m

" Visually Select a method / class and execute it by hitting 'Ctrl+h'

python << EOL

import vim

def evaluate_range():
    eval(compile('\n'.join(vim.current.range), '', 'exec'), globals())

EOL
