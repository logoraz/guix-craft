#!/bin/sh
# Deploy Lem Config

# TODO - Once I learn guile well enough, will use Guix as a deploy agent for this...

# Check if directory or directory link dne then create.
lem=~/.config/lem/
if [ ! -d $lem ]; then
    echo "~/.config/lem/ does not exist - creating..."
    mkdir ~/.config/lem/
fi

# Initialization File
ln -s ~/repos/guix-craft/config/lem/init.lisp \
   ~/.config/lem/init.lisp

ln -s ~/repos/guix-craft/config/lem/lem-icon.png \
   ~/.config/lem/lem-icon.png

# Set Gnome Application Luancher
desktop=~/.local/share/applications/
if [ ! -d $desktop ]; then
    echo "~/.local/share/applications/ does not exist - creating..."
    mkdir ~/.local/share/applications/
fi

ln -s ~/repos/guix-craft/config/lem/lem.desktop \
   ~/.local/share/applications/lem.desktop

# Lem Utilities
lisp=~/.config/lem/lisp/
if [ ! -d $lisp ]; then
    echo "~/.config/lem/lisp/ does not exist - creating..."
    mkdir ~/.config/lem/lisp/
fi

ln -s ~/repos/guix-craft/config/lem/lisp/time-stamp.lisp \
    ~/.config/lem/lisp/time-stamp.lisp

ln -s ~/repos/guix-craft/config/lem/lisp/utilities.lisp \
    ~/.config/lem/lisp/utilities.lisp

ln -s ~/repos/guix-craft/config/lem/lisp/file-prompt.lisp \
    ~/.config/lem/lisp/file-prompt.lisp
