#!/bin/bash

mkdir -p ~/tmp/packages/packer
cd ~/tmp/packages/packer
wget http://aur.archlinux.org/packages/pa/packer/PKGBUILD
makepkg
sudo pacman -U ~/tmp/packages/packer/packer-*.pkg.tar.xz
packer -S spacman
packer -S ttf-ms-fonts
