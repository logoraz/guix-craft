#!/bin/sh
# Deploy Guix Config

# TODO - Once I learn guile well enough, will use Guix as a deploy agent for this...

# Check if directory exists else create.
guix_config=~/.config/guix/
if [ ! -d $guix_config ]; then
    echo "~/.config/guix/ does not exist - creating..."
    mkdir $guix_config
fi

# Guix OS (System + Home + Channels)
ln -f ~/repos/guix-craft/guix-config.scm \
   ~/.config/guix/guix-config.scm

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
