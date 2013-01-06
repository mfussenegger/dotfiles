#!/usr/bin/env python2
# -*- coding: utf-8 -*-

"""cli to generate passwords from a master password """

import sys
from Crypto.Cipher import Blowfish
from argh import ArghParser, command
from getpass import getpass
from base64 import encodestring


@command
def getpw(name):
    try:
        password = getpass()
    except KeyboardInterrupt:
        sys.exit()
    encrypter = Blowfish.new(password)

    name = _ensure_multiple8(name)
    pw = encodestring(encrypter.encrypt(name)).strip('\n')
    return pw


def _ensure_multiple8(name):
    """Add filler chars to make sure the len of name is a multiple of 8"""
    nchars = 8 - len(name) % 8
    return 2 * (name + nchars * 'X')


def main():
    p = ArghParser()
    p.set_default_command(getpw)
    p.dispatch()


if __name__ == '__main__':
    main()
