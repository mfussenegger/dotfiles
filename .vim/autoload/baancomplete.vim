"baancomplete.vim - Omni Completion for baan scripts
" Maintainer:   Mathias Fussenegger < f.mathias {at} zignar.net >
" Version: 0.1
" Last Updated: 2011-09-18
"
" The vimscript part is taken from pythoncomplete.vim

if !has('python')
    echo 'Error: Required vim compiled with +python'
    finish
endif

function! baancomplete#Complete(findstart, base)
    "findstart = 1 when we need to get the text length
    if a:findstart == 1
        let line = getline('.')
        let idx = col('.')
        while idx > 0
            let idx -= 1
            let c = line[idx]
            if c =~ '\w'
                continue
            elseif ! c =~ '\.'
                let idx = -1
                break
            else
                break
            endif
        endwhile

        return idx
    "findstart = 0 when we need to return the list of completions
    else
        "vim no longer moves the cursor upon completion... fix that
        let line = getline('.')
        let idx = col('.')
        let cword = ''
        while idx > 0
            let idx -= 1
            let c = line[idx]
            if c =~ '\w' || c =~ '\.'
                let cword = c . cword
                continue
            elseif strlen(cword) > 0 || idx == 0
                break
            endif
        endwhile
        execute "python vimcomplete('" . cword . "', '" . a:base . "')"
        return g:baancomplete_completions
    endif
endfunction

function! s:DefPython()
python << PYTHONEOF

import vim, os, pickle, sqlite3

debugstmts = []
def dbg(s):
    debugstmts.append(s)
def showdbg():
    for d in debugstmts:
        print 'DBG: %s' % d


vi_home = os.path.join(os.environ.get('HOME'), '.vim', 'plugin')
api_file = os.path.join(vi_home, 'baancomplete_api.sqlite')
api_file_exists = os.path.isfile(api_file)

#with open(os.path.join(vi_home, 'baancomplete_api.pkl'), 'rb') as api_fi:
#    api = pickle.load(api_fi)

if api_file_exists:
    conn = sqlite3.connect( os.path.join(vi_home, 'baancomplete_api.sqlite'))
    conn.row_factory = sqlite3.Row
    cur = conn.cursor()

def vimcomplete(context, match):
    global debugstmts
    debugstmts = []
    #dbg(context)
    #dbg(match)
    dictstr = '['
    if api_file_exists:
        completions = get_completions(context, match)
    else:
        dbg('api file not found')
        completions = []
    for compl in completions:
        dictstr += '{"word":"%s","abbr":"%s","menu":"%s","info":"%s","icase":0},' % (
            compl['word'], compl['abbr'], compl['menu'], compl['info'])
    if dictstr[-1] == ',':
        dictstr = dictstr[:-1]
    dictstr += ']'
    #dbg(dictstr)
    vim.command('silent let g:baancomplete_completions = %s' % dictstr)

def get_completions(context, match):
    completions = []
    search_term = context + '%'
    cur.execute('select word, menu, info from functions where word like ?', (search_term,))
    #for item in api:
    for item in cur.fetchall():
        #if item['word'].startswith(context):
        word = item['word'][len(context):]
        dbg('%s, %s, %s' % (context, word, item['word']))
        completions.append({
            'menu' : item['menu'],
            'word' : word,
            'abbr' : item['word'],
            'info' : item['info']
        })
    return completions

PYTHONEOF
endfunction

call s:DefPython()
