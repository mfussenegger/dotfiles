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

In order to install the tooling run the ``dev`` playbook::

  ansible-playbook playbooks/dev.yml


Uninstall
=========

There is another script which uses ``stow --delete`` to delete the symlinks::

    ./clean.sh

Keep in mind that this doesn't delete the dotfiles folder itself.
