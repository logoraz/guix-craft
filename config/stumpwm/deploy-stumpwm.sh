#!/bin/sh
# Deploy StumpWM Config

# TODO - Once I learn guile well enough, will use Guix as a deploy agent for this...

# Check if directory or directory link dne then create.
stumpwm=~/.config/stumpwm
if [ ! -d $stumpwm ]; then
    echo "~/.config/stumpwm/ does not exist - creating..."
    mkdir ~/.config/stumpwm/
fi

stumpwm_modules=~/.config/stumpwm/data
if [ ! -d $stumpwm_modules ]; then
    echo "~/.config/stumpwm/modules/ does not exist - creating..."
    mkdir ~/.config/stumpwm/modules/
fi

stumpwm_data=~/.config/stumpwm/data
if [ ! -d $stumpwm_data ]; then
    echo "~/.config/stumpwm/data/ does not exist - creating..."
    mkdir ~/.config/stumpwm/data/
fi


# Initialization File
ln -f ~/repos/guix-craft/config/stumpwm/config.lisp \
   ~/.config/stumpwm/config


# Modules

## auto-start (Xorg)
ln -f ~/repos/guix-craft/config/stumpwm/modules/auto-start.lisp \
   ~/.config/stumpwm/modules/auto-start.lisp

## audio
ln -f ~/repos/guix-craft/config/stumpwm/modules/audio-wpctl.lisp \
   ~/.config/stumpwm/modules/audio-wpctl.lisp

## colors
ln -f ~/repos/guix-craft/config/stumpwm/modules/colors.lisp \
   ~/.config/stumpwm/modules/colors.lisp

## theme
ln -f ~/repos/guix-craft/config/stumpwm/modules/theme.lisp \
   ~/.config/stumpwm/modules/theme.lisp

## modline
ln -f ~/repos/guix-craft/config/stumpwm/modules/modeline.lisp \
   ~/.config/stumpwm/modules/modeline.lisp

## frames/windows
ln -f ~/repos/guix-craft/config/stumpwm/modules/frames.lisp \
   ~/.config/stumpwm/modules/frames.lisp

## keybindings
ln -f ~/repos/guix-craft/config/stumpwm/modules/keybindings.lisp \
   ~/.config/stumpwm/modules/keybindings.lisp

## commands
ln -f ~/repos/guix-craft/config/stumpwm/modules/commands.lisp \
   ~/.config/stumpwm/modules/commands.lisp

## utilities
ln -f ~/repos/guix-craft/config/stumpwm/modules/utilities.lisp \
   ~/.config/stumpwm/modules/utilities.lisp

# StumpWM Contrib not available in Guix

ln -f ~/repos/guix-craft/config/stumpwm/modules/end-session.lisp \
   ~/.config/stumpwm/modules/end-session.lisp
