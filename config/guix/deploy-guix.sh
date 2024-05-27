#!/bin/sh
# Deploy Guix

# TODO - Once I learn guile well enough, will use Guix as a deploy agent for this...

# Check if guix directory exists else create it.
guix=~/.config/guix/
if [ ! -d $guix ]; then
    echo "~/.config/guix/ does not exist - creating..."
    mkdir ~/.config/guix/
fi

# channels.scm
ln -s ~/repos/guix-craft/config/guix/channels.scm \
   ~/.config/guix/channels.scm
