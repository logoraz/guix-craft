#!/bin/sh
# Deploy Xorg Config

# TODO - Once I learn guile well enough, will use Guix as a deploy agent for this...

# .icons directory -> custom cursor
icons=~/.icons
if [ ! -d $icons ] && [ ! -L $icons ]; then
    echo "~/.icons/ does not exist - creating..."
    ln -s ~/repos/guix-craft/config/xorg/dot-icons \
    ~/.icons
fi

# Check if correct directory scaffold exist else create it.
xorg=~/.config/xorg
if [ ! -d $xorg ] && [ ! -L $xorg ]; then
    echo "~/.config/xorg/ does not exist - creating..."
    ln -s ~/repos/guix-craft/config/xorg \
       ~/.config/xorg
fi

# .Xdefaults
ln -s ~/repos/guix-craft/config/xorg/dot-Xdefaults \
   ~/.Xdefaults

# .Xresources
ln -s ~/repos/guix-craft/config/xorg/dot-Xresources \
   ~/.Xresources
