My public dotfiles.

Installation
============

::

    git clone --recursive https://github.com/mfussenegger/dotfiles.git
    cd dotfiles
    ./bootstrap.sh


``bootstrap.sh`` will symlink all necessary files to the home directory.

To do so the script uses `GNU stow
<https://www.gnu.org/software/stow/stow.html>`_. It has to be installed in
order for the `bootstrap.sh` script to work.


hopp.py
-------

The ``bootstrap.sh`` script will call ``hopp.py`` (included in the ``bin/bin``
directory) to download all vim-plugins and setup a couple of virtualenvs.

If you want to know what exactly is being installed by ``hopp.py`` take a look at
the configuration files (in the ``hopp`` folder)

Uninstall
=========

There is another script which uses ``stow --delete`` to delete the symlinks::

    ./clean.sh

Keep in mind that this doesn't delete the dotfiles folder itself.
