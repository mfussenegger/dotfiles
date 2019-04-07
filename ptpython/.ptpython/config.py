
# See https://github.com/prompt-toolkit/ptpython/blob/master/examples/ptpython_config/config.py

def configure(repl):
    repl.vi_mode = True
    repl.show_signature = True
    repl.show_docstring = True
    repl.enable_open_in_editor = True
    repl.confirm_exit = False
    repl.enable_auto_suggest = True
    repl.complete_while_typing = True
    repl.use_code_colorscheme('monokai')
