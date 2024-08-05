#!/bin/sh
# Deploy StumpWM Config

# TODO - Once I learn guile well enough, will use Guix as a deploy agent for this...

# Check if directory or directory link dne then create.
stumpwm=~/.config/stumpwm/
if [ ! -d $stumpwm ]; then
    echo "~/.config/stumpwm/ does not exist - creating..."
    mkdir $stumpwm
fi

stumpwm_modules=~/.config/stumpwm/modules/
if [ ! -d $stumpwm_modules ]; then
    echo "~/.config/stumpwm/modules/ does not exist - creating..."
    mkdir $stumpwm_modules
fi

stumpwm_data=~/.config/stumpwm/data/
if [ ! -d $stumpwm_data ]; then
    echo "~/.config/stumpwm/data/ does not exist - creating..."
    mkdir $stumpwm_data
fi


# Initialization File
ln -f ~/repos/guix-craft/config/stumpwm/config.lisp \
   ~/.config/stumpwm/config


# Modules (StumpWM settings/configuration files)

## auto-start (Xorg)
ln -f ~/repos/guix-craft/config/stumpwm/modules/auto-start.lisp \
   ~/.config/stumpwm/modules/auto-start.lisp

## commands
ln -f ~/repos/guix-craft/config/stumpwm/modules/commands.lisp \
   ~/.config/stumpwm/modules/commands.lisp

## utilities
ln -f ~/repos/guix-craft/config/stumpwm/modules/utilities.lisp \
   ~/.config/stumpwm/modules/utilities.lisp

## frames/windows
ln -f ~/repos/guix-craft/config/stumpwm/modules/frames.lisp \
   ~/.config/stumpwm/modules/frames.lisp

## keybindings
ln -f ~/repos/guix-craft/config/stumpwm/modules/keybindings.lisp \
   ~/.config/stumpwm/modules/keybindings.lisp

## colors
ln -f ~/repos/guix-craft/config/stumpwm/modules/colors.lisp \
   ~/.config/stumpwm/modules/colors.lisp

## theme
ln -f ~/repos/guix-craft/config/stumpwm/modules/theme.lisp \
   ~/.config/stumpwm/modules/theme.lisp

## modline
ln -f ~/repos/guix-craft/config/stumpwm/modules/modeline.lisp \
   ~/.config/stumpwm/modules/modeline.lisp


# Stumpwm-contrib Packages
stumpwm_contrib=~/.local/share/common-lisp/stumpwm-contrib/
if [ ! -d $stumpwm_contrib ]; then
    echo "~/.local/share/stumpwm/contrib/ does not exist - creating..."
    mkdir $stumpwm_contrib
fi

## util & modeline: wpctl - Wireplumber Audio Controls for StumpWM
ln -s ~/repos/guix-craft/config/stumpwm/libraries/wpctl \
   ~/.local/share/common-lisp/stumpwm-contrib/wpctl

## util: bluetooth
ln -s ~/repos/guix-craft/config/stumpwm/libraries/bluetooth/ \
   ~/.local/share/common-lisp/stumpwm-contrib/bluetooth

## util: end-session
ln -s ~/repos/guix-craft/config/stumpwm/libraries/end-session \
   ~/.local/share/common-lisp/stumpwm-contrib/end-session

## util: screenshot
ln -s ~/repos/guix-craft/config/stumpwm/libraries/screenshot \
   ~/.local/share/common-lisp/stumpwm-contrib/screenshot
