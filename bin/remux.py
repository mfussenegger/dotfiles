#!/usr/bin/env python
# -*- coding: utf-8 -*-

import os
import re
import sys

from argh import ArghParser, command
from subprocess import check_call as call


@command
def remux(filename):
    nfilename = re.sub('[^-.a-zA-Z0-9_]', '', filename)
    os.rename(filename, nfilename)
    filename = nfilename
    call(['projectx', filename])

    os.remove(re.sub('\.m2t$', '_log.txt', filename))

    audiofile = None
    audio_ac3 = re.sub('\.m2t$', '.ac3', filename)
    audio_mp2 = re.sub('\.m2t$', '.mp2', filename)

    if os.path.exists(audio_ac3):
        audiofile = audio_ac3

    if os.path.exists(audio_mp2):
        if not audiofile:
            audiofile = audio_mp2
        else:
            os.remove(audio_mp2)

    videofile = re.sub('\.m2t$', '.m2v', filename)

    mplexed = re.sub(' ', '_', filename)
    mplexed = re.sub('\.m2t', '.mpg', mplexed)

    call(['mplex', '-f', '3', '-o', mplexed, audiofile, videofile])

    os.remove(audiofile)
    os.remove(videofile)

    cutfile = mplexed + '_cut'
    idxfile = mplexed + '.idx'
    infotext = 'Starting avidemux. Save file as {0}'.format(cutfile)
    call(['zenity', '--info', '--text="{0}"'.format(infotext)])
    call(['avidemux2_gtk', mplexed])

    if not os.path.exists(cutfile):
        print('{0} not found.'.format(cutfile))
        sys.exit(1)

    call(['HandBrakeCLI',
               '-Z',
               'Normal',
               '-i',
               cutfile,
               '-o',
               re.sub('\.mpg$', '.mp4', mplexed)])

    os.remove(cutfile)
    os.remove(mplexed)
    os.remove(idxfile)


def main():
    p = ArghParser()
    p.add_commands([remux])
    p.dispatch()


if __name__ == '__main__':
    main()
