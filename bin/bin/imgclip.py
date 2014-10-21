#!/usr/bin/env python
# -*- coding: utf-8 -*-

from gi.repository import Gtk, Gdk
import sys

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
