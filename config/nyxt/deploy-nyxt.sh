#!/bin/sh
# Deploy Nyxt Config

# TODO - Once I learn guile well enough, will use Guix as a deploy agent for this...

# Check if directory or directory link dne then create.
nyxt=~/.config/nyxt/
if [ ! -d $nyxt ]; then
    echo "~/.config/nyxt/ does not exist - creating..."
    mkdir ~/.config/nyxt/
fi

# Initialization File
ln -s ~/repos/guix-craft/config/nyxt/config.lisp \
   ~/.config/nyxt/config.lisp

# Utilities
ln -s ~/repos/guix-craft/config/nyxt/utilities.lisp \
   ~/.config/nyxt/utilities.lisp

# Password Management
ln -s ~/repos/guix-craft/config/nyxt/passwords.lisp \
   ~/.config/nyxt/passwords.lisp

# Themes
ln -s ~/repos/guix-craft/config/nyxt/theme.lisp \
   ~/.config/nyxt/theme.lisp
