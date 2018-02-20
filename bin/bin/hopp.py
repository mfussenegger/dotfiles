#!/usr/bin/env python
# -*- coding: utf-8 -*-

"""
A tool to clone git repos, bootstrap virtualenvs and download files.
Kind of a generic bootstrapping tool or plugin manager.

Inspired by https://github.com/buildinspace/peru but with some differences:
    - no dependencies (json instead of yaml configuration)
    - no version pinning - always bleeding edge
    - can only fetch/update things
    - only git suport
    - supports unsafe cmd execution via shell subprocess calls

Usage::

    hopp.py hopp.json [ , ... ]

(There is an example hopp.json file in this repo as example)
"""

import os
import json
import venv
from argparse import ArgumentParser
from subprocess import PIPE, run
from urllib.request import urlopen


def _exec_cmds(location, cmds=None):
    if not cmds:
        return
    if os.path.isfile(location):
        location = os.path.dirname(location)

    if isinstance(cmds, list):
        for cmd in cmds:
            run(cmd, shell=True, cwd=location)
    else:
        run(cmds, shell=True, cwd=location)


def virtualenv(location, virtualenv, cmds=None):
    """ create a virtualenv with the given dependencies in the target location

        {
            "location": "target/path",
            "virtualenv": ["dep1", "dep2"]
        }
    """
    if not os.path.exists(location):
        venv.create(location, with_pip=True)

    vpython = os.path.join(location, 'bin', 'python')

    pip_install = [vpython, '-m', 'pip', 'install', '--upgrade']
    run(pip_install + ['pip'])

    dependencies = virtualenv
    run(pip_install + dependencies)

    _exec_cmds(location, cmds)


def curl(location, curl, cmds=None):
    """ load an entry like

        {
            "location": "target/path",
            "curl": "http://example.com/source.vim"
        }
    """
    location = location
    source = curl
    resp = urlopen(source)
    if os.path.isdir(location):
        location = os.path.join(location, os.path.basename(source))
    elif location.endswith('/'):
        os.mkdir(location)
        location = os.path.join(location, os.path.basename(source))
    with open(location, 'wb') as f:
        f.write(resp.read())
    _exec_cmds(location, cmds)


def _pull(location, cmds=None):
    cmd = ['git', 'pull']
    p = run(cmd, cwd=location, stdout=PIPE)
    if b'Updating ' not in p.stdout:
        cmd = ['git', 'submodule', 'update', '--init', '--recursive']
        run(cmd, cwd=location)
        _exec_cmds(location, cmds)


def git(location, git, branch=None, cmds=None):
    """ load a git entry:

        {
            "location": "vim/.vim/bundle/vimproc",
            "git": "https://github.com/Shougo/vimproc.vim.git",
        }
    """

    # location:
    # - .vim/bundle      (exists)           -> git clone (create basename of url)
    # - .vim/bundle/foo  (with .git)        -> git pull
    # - .vim/bundle/foo  (doesn't exist)    -> git clone (creates foo)

    repo = os.path.basename(git)
    if repo.endswith('.git'):
        repo = repo[:-4]
    if os.path.exists(location):
        if os.path.exists(os.path.join(location, '.git')):
            return _pull(location, cmds=cmds)

        location = os.path.join(location, repo)
        if os.path.exists(os.path.join(location, '.git')):
            return _pull(location, cmds=cmds)
    source = git
    cmd = ['git', 'clone', '--depth', '1', '--recursive']
    if branch:
        cmd += ['-b', branch]
    cmd += [source, location]
    run(cmd)
    _exec_cmds(location, cmds)


def github(location, github, cmds=None):
    """ shortcut for git """
    url_tmpl = 'https://github.com/{0}.git'
    if not isinstance(github, list):
        url = url_tmpl.format(github)
        git(location=location, git=url, cmds=cmds)
    else:
        urls = [url_tmpl.format(u) for u in github]

        def g(url):
            git(location=location, git=url, cmds=cmds)
        for url in urls:
            g(url)


def zsh(location, zsh, cmds=None):
    filenames = []
    for url in zsh:
        curl(location, url)
        filenames.append(os.path.basename(url))
    zshrc = os.path.expanduser('~/.zshrc')
    entries = ['source ~/.zsh/plugins/{}'.format(fn) for fn in filenames]
    with open(zshrc, 'r') as f:
        for line in f:
            for entry in entries:
                if entry in line:
                    entries.remove(entry)
    with open(zshrc, 'a') as f:
        for entry in entries:
            f.write('{entry}\n'.format(entry=entry))


loaders = {
    'curl': curl,
    'git': git,
    'github': github,
    'virtualenv': virtualenv,
    'zsh': zsh
}


def try_load_entry(entry):
    if 'location' in entry:
        before = entry['location']
        entry['location'] = os.path.abspath(os.path.expanduser(entry['location']))
        if before.endswith('/'):
            entry['location'] += '/'
    for key in entry:
        if key in loaders:
            return loaders[key](**entry)
    print('Could not load ' + str(entry))


def main():
    p = ArgumentParser('hopp.py', 'bootstrap stuff')
    p.add_argument(
        'config', type=str, nargs='+', help=('One or more configuration files'))
    args = p.parse_args()
    if isinstance(args.config, list):
        configs = args.config
    else:
        configs = [args.config]

    entries = []
    for config in configs:
        with open(config, 'r') as f:
            entries += json.load(f)

    for entry in entries:
        try_load_entry(entry)


if __name__ == '__main__':
    main()
