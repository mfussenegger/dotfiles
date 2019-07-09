#!/usr/bin/env python
# -*- coding: utf-8 -*-


import os
import re
import json
import typing
from urllib.request import urlretrieve
from sh import i3_msg
from sh import xdg_open
from sh import dmenu as shdmenu
from sh import cut
from sh import xset
from sh import sed
from sh import xrandr
from sh import killall
from sh import nvr
from sh import setxkbmap
from sh import ErrorReturnCode_1
from sh import xdotool
from sh import mpc
try:
    from sh import virsh
    # enable users in libvirt group to use virsh without sudo
    virsh = virsh.bake(_env={'LIBVIRT_DEFAULT_URI': 'qemu:///system'})
except ImportError:
    pass
try:
    from sh import vboxmanage
except ImportError:
    pass


shdmenu = shdmenu.bake(
    '-l', 30, '-fn',
    '-*-terminus-medium-*-*-*-14-140-*-*-*-*-*-*'
)


def dmenu(args):
    if isinstance(args, typing.Iterable):
        args = '\n'.join(args)
    return shdmenu(_in=args)


def grep(output, pattern):
    for line in output:
        if pattern in line:
            yield line


def output(p):
    return p.stdout.decode('utf-8').strip()


OUTPUT_ACTIVE_REX = re.compile(
    r'.* connected( primary)? (\d)+x(\d)+\+\d+\+\d+ .*')


def cmd_xrandr_on():
    outputs = grep(xrandr(), ' connected')
    outputs = [o.split(' ')[0] for o in outputs
               if not OUTPUT_ACTIVE_REX.match(o)]
    if len(outputs) > 1:
        outputs = output(dmenu(outputs)).split('\n')
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
    choice = output(dmenu(choices))
    option, active_output = choice.split(' ')
    xrandr('--output', selected_output, option, active_output, '--auto')


def _get_active_outputs():
    outputs = grep(xrandr(), ' connected')
    return [o.split(' ')[0] for o in outputs
            if OUTPUT_ACTIVE_REX.match(o)]


def cmd_xrandr_off():
    outputs = _get_active_outputs()
    o = output(dmenu(outputs))
    xrandr('--output', o.split(' ')[0], '--off')


def cmd_vbox_launch():
    o = output(shdmenu(cut(vboxmanage('list', 'vms'), '-d"', '-f2')))
    vboxmanage('-q', 'startvm', o, '--type', 'gui')


def cmd_virsh_launch():
    vms = output(virsh('list', '--all')).split('\n')
    # virsh output:
    #
    #  Id    Name                           State
    # ----------------------------------------------------
    #  -     foobar                         shut off
    vms = (vm.strip() for vm in vms[2:])
    vms = (re.split(' {2,}', i) for i in vms)
    names = [i[1] for i in vms]
    name = output(dmenu(names))
    virsh('start', name)


def _change_vim_color_scheme(colorscheme, background):
    vimrc_path = os.path.expanduser('~/.config/nvim/options.vim')

    sed_cmd = 's/colorscheme {old_scheme}/colorscheme {new_scheme}/g'
    sed_cmd = sed_cmd.format(old_scheme=colorscheme[0], new_scheme=colorscheme[1])
    sed('--follow-symlinks', '-i', sed_cmd, vimrc_path)

    sed_cmd = 's/background={old_bg}/background={new_bg}/g'
    sed_cmd = sed_cmd.format(old_bg=background[0], new_bg=background[1])
    sed('--follow-symlinks', '-i', sed_cmd, vimrc_path)

    change_color = '<Esc>:set background={new_bg}<CR>:colorscheme {new_scheme}<CR>'
    change_color = change_color.format(new_bg=background[1], new_scheme=colorscheme[1])
    try:
        for server in nvr('--serverlist'):
            nvr('--servername', server.strip(), '--remote-send', change_color)
    except ErrorReturnCode_1:
        pass


def _set_termite_config(config_name):
    config_dir = os.path.expanduser('~/.config/termite/')
    if not os.path.exists(os.path.join(config_dir, config_name)):
        return
    dst = os.path.join(config_dir, 'config')
    if os.path.exists(dst):
        os.remove(dst)
    dir_fd = os.open(config_dir, flags=os.O_DIRECTORY)
    os.symlink(config_name, 'config', dir_fd=dir_fd)
    killall('-s', 'USR1', 'termite')


def cmd_call():
    filename = os.path.expanduser('~/.config/dm/contacts.json')
    with open(filename, encoding='utf-8') as f:
        contacts = json.load(f)
    o = output(dmenu(contacts.keys()))
    xdg_open(contacts[o])


def cmd_pres_off():
    xset('s', 'on')
    xset('+dpms')
    _change_vim_color_scheme(('default', 'gruvbox'), ('light', 'dark'))
    _set_termite_config('config_dark')


def cmd_pres_on():
    xset('s', 'off')
    xset('-dpms')
    _change_vim_color_scheme(('gruvbox', 'default'), ('dark', 'light'))
    _set_termite_config('config_light')


def cmd_i3bar_invisible():
    i3_msg('bar mode invisible')


def cmd_i3bar_dock():
    i3_msg('bar mode dock')


def cmd_keyboard():
    xset('r', 'rate', 200, 40)
    setxkbmap(
        '-layout', 'de',
        '-variant', 'nodeadkeys',
        '-option', 'caps:escape'
    )


def cmd_emoji():
    emoji_json = os.path.expanduser('~/.config/dm/emoji.json')
    emoji_source = 'https://raw.githubusercontent.com/github/gemoji/master/db/emoji.json'
    if not os.path.exists(emoji_json):
        urlretrieve(emoji_source, filename=emoji_json)
    with open(emoji_json, encoding='utf-8') as f:
        emoji = json.load(f)
    o = output(dmenu((i['description'] for i in emoji if 'emoji' in i)))
    xdotool(
        'type',
        '--delay', '100',
        next((i['emoji'] for i in emoji if i['description'] == o)))


def cmd_mpc():
    songs = output(mpc('playlist', '-f', '%position% ¦ %artist% - %title%')).split('\n')
    song = dmenu(songs)
    nr = song.split(' ¦ ')[0]
    mpc('play', nr)



def main():
    prefix = 'cmd_'
    g = globals()
    commands = []
    for k in g:
        if k.startswith(prefix):
            commands.append(k[len(prefix):].replace('_', ' '))
    o = output(dmenu(commands))
    g[prefix + o.replace(' ', '_')]()


if __name__ == '__main__':
    main()
