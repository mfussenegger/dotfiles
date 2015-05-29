#!/usr/bin/env python
# -*- coding: utf-8 -*-


import os
import json
from subprocess import Popen, PIPE
from urllib.request import urlopen
from multiprocessing import Pool


def _build(entry):
    if not 'build' in entry:
        return
    # shell=True -> scary!
    location = entry['location']
    if os.path.isfile(location):
        location = os.path.dirname(location)
    p = Popen(entry['build'], shell=True, cwd=location)
    p.wait()


def virtualenv(entry):
    """ create a virtualenv with the given dependencies in the target location

        {
            "location": "target/path",
            "virtualenv": ["dep1", "dep2"]
        }
    """
    location = os.path.abspath(os.path.expanduser(entry['location']))
    dependencies = entry['virtualenv']
    cmd = ['python', '-m', 'venv', location]
    p = Popen(cmd)
    p.wait()
    vpython = os.path.join(location, 'bin', 'python')

    pip = [vpython, '-m', 'pip', 'install', '--upgrade']
    p = Popen(pip + ['pip'])
    p.wait()
    p = Popen(pip + dependencies)
    p.wait()


def curl(entry):
    """ load an entry like

        {
            "location": "target/path",
            "curl": "http://example.com/source.vim"
        }
    """
    location = entry['location']
    source = entry['curl']
    resp = urlopen(source)
    if os.path.isdir(location):
        location = os.path.join(location, os.path.basename(source))
    elif location.endswith('/'):
        os.mkdir(location)
        location = os.path.join(location, os.path.basename(source))
    if os.path.exists(location):
        print('Skip {}, there is already something at the location'.format(entry))
        return
    with open(location, 'wb') as f:
        f.write(resp.read())
    _build(entry)


def git(entry):
    """ load a git entry:

        {
            "location": "vim/.vim/bundle/vimproc",
            "git": "https://github.com/Shougo/vimproc.vim.git",
        }
    """
    location = entry['location']
    if os.path.exists(location):
        cmd = ['git', 'pull', 'origin', 'master']
        p = Popen(cmd, cwd=location, stdout=PIPE)
        stdout, stderr = p.communicate()
        if b'Already up-to-date' not in stdout:
            cmd = ['git', 'submodule', 'update', '--init', '--recursive']
            p = Popen(cmd, cwd=location)
            _build(entry)
    else:
        source = entry['git']
        cmd = ['git', 'clone', '--depth', '1', '--recursive']
        branch = entry.get('branch', None)
        if branch:
            cmd += ['-b', branch]
        cmd += [source, location]
        p = Popen(cmd)
        p.wait()
        _build(entry)


def github(entry):
    """ shortcut for git """
    entry['git'] = 'https://github.com/{0}.git'.format(entry['github'])
    del entry['github']
    git(entry)



loaders = {
    'curl': curl,
    'git': git,
    'github': github,
    'virtualenv': virtualenv
}


def try_load_entry(entry):
    if 'location' in entry:
        entry['location'] = os.path.expanduser(entry['location'])
    for key in entry:
        if key in loaders:
            return loaders[key](entry)
    print('Could not load ' + str(entry))


def main():
    f = os.path.join(os.curdir, 'hopp.json')
    entries = json.load(open(f, 'r'))

    with Pool(16) as pool:
        pool.map(try_load_entry, entries)


if __name__ == '__main__':
    main()
