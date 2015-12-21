#!/usr/bin/env python
# -*- coding: utf-8 -*-


import os
import re
from sh import dmenu
from sh import echo
from sh import cut
from sh import xset
from sh import xrdb
from sh import sed
from sh import xrandr
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


def cmd_pres_off():
    vimrc_path = os.path.expanduser('~/.vimrc')
    xset('s', 'on')
    xset('+dpms')
    xrdb(os.path.expanduser('~/.Xresources'))
    sed('-i', 's/colorscheme github/colorscheme zenburn/g', vimrc_path)
    sed('-i', 's/background=light/background=dark/g', vimrc_path)


def cmd_pres_on():
    vimrc_path = os.path.expanduser('~/.vimrc')
    xset('s', 'off')
    xset('-dpms')
    xrdb(os.path.expanduser('~/dotfiles/shell/.Xresources-presentation'))
    sed('-i', 's/colorscheme zenburn/colorscheme github/g', vimrc_path)
    sed('-i', 's/background=dark/background=light/g', vimrc_path)


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
