#!/bin/sh
# Deploy Lem Config

# TODO - Once I learn guile well enough, will use Guix as a deploy agent for this...

# Check if directory or directory link dne then create.
common_lisp=/home/logoraz/common-lisp/
if [ ! -d $common_lisp ]; then
    echo "~/common-lisp/ does not exist - creating..."
    mkdir ~/common-lisp/
fi

# SBCL Init file
ln -s ~/repos/guix-craft/config/common-lisp/dot-sbclrc.lisp \
   ~/.sbclrc

# Quicklisp
quicklisp=/home/logoraz/.config/quicklisp
if [ ! -d $quicklisp ] && [ ! -L $quicklisp ]; then
    echo "/home/logoraz/.config/quicklisp/ does not exist - creating..."
    ln -s ~/repos/guix-craft/config/common-lisp/quicklisp \
       /home/logoraz/.config/quicklisp
fi
