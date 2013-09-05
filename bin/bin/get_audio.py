#!/usr/bin/env python
# -*- coding: utf-8 -*-

import sys
import os
from sh import youtube_dl, ffmpeg

if len(sys.argv) < 3:
    sys.exit("Usage: get_audio.py url output")

url = sys.argv[1]
output = sys.argv[2].replace(' - ', '_').replace(' ', '-')
output = output.replace("'", '').replace(',', '')
if not output.endswith('.mp3'):
    output += '.mp3'
youtube_dl(url)

filename = url.split('/')[-1]
flv = filename + '.flv'
mp4 = filename + '.mp4'

if os.path.isfile(flv):
    filename = flv
elif os.path.isfile(mp4):
    filename = mp4
else:
    sys.exit("Download failed?")

ffmpeg('-i', filename, '-ac', '1', '-ab', '192k', '-vn', '-f', 'mp3', output)

if os.path.isfile(output):
    os.remove(filename)
