#!/bin/sh
# Deploy Guix Config

# TODO - Once I learn guile well enough, will use Guix as a deploy agent for this...

# Check if directory exists else create.
guix_config=~/.config/guix/
if [ ! -d $guix_config ]; then
    echo "~/.config/guix/ does not exist - creating..."
    mkdir $guix_config
fi

# System
ln -f ~/repos/guix-craft/system.scm \
   ~/.config/guix/system.scm

# Home
ln -f ~/repos/guix-craft/home.scm \
   ~/.config/guix/home.scm

# Channels
ln -f ~/repos/guix-craft/config/guix/channels.scm \
   ~/.config/guix/channels.scm

# Bash Profiles
ln -f ~/repos/guix-craft/config/guix/dot-bashrc.sh \
   ~/.config/guix/dot-bashrc.sh

ln -f ~/repos/guix-craft/config/guix/dot-bash_profile.sh \
   ~/.config/guix/dot-bash_profile.sh

# Manifests
guix_manifests=~/.config/guix/manifests
if [ ! -d $guix_manifests ] && [ ! -L $guix_manifests ]; then
    echo "~/.config/guix/manifests does not exist - creating..."
    ln -s ~/repos/guix-craft/config/guix/manifests \
       $guix_manifests
fi
