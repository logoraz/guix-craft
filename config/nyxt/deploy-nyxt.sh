#!/bin/sh
# Deploy Nyxt Config

# TODO - Once I learn guile well enough, will use Guix as a deploy agent for this...

# Check if directory or directory link dne then create.
nyxt=~/.config/nyxt/
if [ ! -d $nyxt ]; then
    echo "~/.config/nyxt/ does not exist - creating..."
    mkdir ~/.config/nyxt/
fi

# StumpWM Start Script
ln -f ~/repos/guix-craft/config/nyxt/start-nyxt.sh \
   ~/.config/nyxt/start-nyxt.sh

# Initialization File
ln -f ~/repos/guix-craft/config/nyxt/config.lisp \
   ~/.config/nyxt/config.lisp

# Themes
ln -f ~/repos/guix-craft/config/nyxt/theme.lisp \
   ~/.config/nyxt/theme.lisp

# Utilities
ln -f ~/repos/guix-craft/config/nyxt/utilities.lisp \
   ~/.config/nyxt/utilities.lisp

# Password Management
ln -f ~/repos/guix-craft/config/nyxt/passwords.lisp \
   ~/.config/nyxt/passwords.lisp

ln -f ~/repos/guix-craft/config/nyxt/passwords-dev.lisp \
   ~/.config/nyxt/passwords-dev.lisp
