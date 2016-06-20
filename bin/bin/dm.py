#!/usr/bin/env python
# -*- coding: utf-8 -*-


import os
import re
import json
from sh import xdg_open
from sh import dmenu
from sh import echo
from sh import cut
from sh import xset
from sh import sed
from sh import xrandr
from sh import ln
from sh import killall
from sh import vim
try:
    from sh import vboxmanage
except ImportError:
    pass

dmenu = dmenu.bake(
    '-l', 30, '-fn',
    '-*-terminus-medium-*-*-*-14-140-*-*-*-*-*-*'
)


def grep(output, pattern):
    for line in output:
        if pattern in line:
            yield line


def output(p):
    return p.stdout.decode('utf-8').strip()


OUTPUT_ACTIVE_REX = re.compile(
    '.* connected( primary)? (\d)+x(\d)+\+\d+\+\d+ .*')


def cmd_xrandr_on():
    outputs = grep(xrandr(), ' connected')
    outputs = [o.split(' ')[0] for o in outputs
               if not OUTPUT_ACTIVE_REX.match(o)]
    if len(outputs) > 1:
        outputs = output(dmenu(echo('\n'.join(outputs)))).split('\n')
    if not outputs:
        return
    selected_output = outputs[0]
    active_outputs = _get_active_outputs()
    if not active_outputs:
        return
    choices = []
    for active_output in active_outputs:
        choices.append('--left-of ' + active_output)
        choices.append('--right-of ' + active_output)
        choices.append('--below ' + active_output)
        choices.append('--above ' + active_output)
        choices.append('--same-as ' + active_output)
    choice = output(dmenu(echo('\n'.join(choices))))
    option, active_output = choice.split(' ')
    xrandr('--output', selected_output, option, active_output, '--auto')


def _get_active_outputs():
    outputs = grep(xrandr(), ' connected')
    return [o.split(' ')[0] for o in outputs
            if OUTPUT_ACTIVE_REX.match(o)]


def cmd_xrandr_off():
    outputs = _get_active_outputs()
    o = output(dmenu(echo('\n'.join(outputs))))
    xrandr('--output', o.split(' ')[0], '--off')


def cmd_vbox_launch():
    o = output(dmenu(cut(vboxmanage('list', 'vms'), '-d"', '-f2')))
    vboxmanage('-q', 'startvm', o, '--type', 'gui')


def _change_vim_color_scheme(colorscheme, background):
    vimrc_path = os.path.expanduser('~/.vimrc')

    sed_cmd = 's/colorscheme {old_scheme}/colorscheme {new_scheme}/g'
    sed_cmd = sed_cmd.format(old_scheme=colorscheme[0], new_scheme=colorscheme[1])
    sed('-i', sed_cmd, vimrc_path)

    sed_cmd = 's/background={old_bg}/background={new_bg}/g'
    sed_cmd = sed_cmd.format(old_bg=background[0], new_bg=background[1])
    sed('-i', sed_cmd, vimrc_path)

    change_color = '<Esc>:set background={new_bg}<CR>:colorscheme {new_scheme}<CR>'
    change_color = change_color.format(new_bg=background[1], new_scheme=colorscheme[1])
    for server in vim('--serverlist'):
        vim('--servername', server.strip(), '--remote-send', change_color)


def _set_termite_config(config_name):
    config_dir = os.path.expanduser('~/.config/termite/')
    if not os.path.exists(os.path.join(config_dir, config_name)):
        return
    ln('-sf', config_name, 'config', _cwd=config_dir)
    killall('-s', 'USR1', 'termite')


def cmd_call():
    filename = os.path.expanduser('~/.config/dm/contacts.json')
    with open(filename, encoding='utf-8') as f:
        contacts = json.load(f)
    o = output(dmenu(echo('\n'.join(contacts.keys()))))
    xdg_open(contacts[o])


def cmd_pres_off():
    xset('s', 'on')
    xset('+dpms')
    _change_vim_color_scheme(('default', 'zenburn'), ('light', 'dark'))
    _set_termite_config('config_dark')


def cmd_pres_on():
    xset('s', 'off')
    xset('-dpms')
    _change_vim_color_scheme(('zenburn', 'default'), ('dark', 'light'))
    _set_termite_config('config_light')


def main():
    prefix = 'cmd_'
    g = globals()
    commands = []
    for k in g:
        if k.startswith(prefix):
            commands.append(k[len(prefix):].replace('_', ' '))
    o = output(dmenu(echo('\n'.join(commands))))
    g[prefix + o.replace(' ', '_')]()


if __name__ == '__main__':
    main()
