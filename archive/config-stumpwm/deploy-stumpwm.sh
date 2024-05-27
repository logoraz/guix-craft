#!/bin/sh
# Deploy StumpWM Config

# TODO - Once I learn guile well enough, will use Guix as a deploy agent for this...

# Check if directory or directory link dne then create.
stumpwm=~/.config/stumpwm
if [ ! -d $stumpwm ] && [ ! -L $stumpwm ]; then
    echo "~/.config/stumpwm/ does not exist - creating..."
    ln -s ~/repos/guix-craft/config/stumpwm \
       ~/.config/stumpwm
fi
