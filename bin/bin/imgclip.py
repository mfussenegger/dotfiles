#!/usr/bin/env python
# -*- coding: utf-8 -*-

"""
utility that uses Gtk to put an image file (given as argument) into the
clipboard making it possible to paste the image in other programs.

Example usage::

    scrot -s /tmp/screenshot.png -e 'imgclip.py $f'

Then CTLR+v in any application to paste the image.


"""

import sys
try:
    import gi
    gi.require_version('Gtk', '3.0')
    from gi.repository import Gtk, Gdk
except ImportError:
    sys.exit('gi.repository not found. Install python-gobject (under Archlinux)')


count = 0


def handle_owner_change(clipboard, event):
    global count
    print('clipboard.owner-change(%r, %r)' % (clipboard, event))
    count += 1
    if count > 1:
        sys.exit(0)

image = Gtk.Image.new_from_file(sys.argv[1])
clipboard = Gtk.Clipboard.get(Gdk.SELECTION_CLIPBOARD)
clipboard.connect('owner-change', handle_owner_change)
clipboard.set_image(image.get_pixbuf())
clipboard.store()
Gtk.main()
