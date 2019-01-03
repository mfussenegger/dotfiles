My public dotfiles.

Installation
============

::

    git clone https://github.com/mfussenegger/dotfiles.git
    cd dotfiles
    ansible-playbook playbooks/aur.yml
    ansible-playbook playbooks/dev.yml -K

This requires `Ansible <https://www.ansible.com/>`_ to symlink the
configurations and install packages + vim plugins.

This is tailored to my needs. For others I recommend to review & cherry-pick
specific roles instead of applying everything.


Stow
====

The folder structure for the configurations is setup so it can be used with
`GNU Stow <https://www.gnu.org/software/stow/>`_. For example::

   stow psql

To symlink all files within psql to your home folder.


Apply ansible roles
===================

To only apply specific roles you can use ``ansible -m import_role`` within the
``playbooks`` folder::

   cd playbooks
   ansible localhost -m import_role -a name=vim

To apply some roles on multiple remote hosts you can use ``ansible-console``.
For example, to initialize the dotfiles on other hosts the following could be
used::

   cd playbooks
   ansible-console yourhosts -K --ssh-extra-args="-o ForwardAgent=yes"
   $ import_role name=dotfiles
   $ import_role name=stow
   $ import_role name=zsh-plugins
   $ import_role name=vim
