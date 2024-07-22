#!/bin/sh
# Deploy Common Lisp Environment

# TODO - Once I learn guile well enough, will use Guix as a deploy agent for this...

# See Advanced Dependencies Management
# |-> ;; https://lispcookbook.github.io/cl-cookbook/getting-started.html
# You can drop Common Lisp projects into any of these folders:
#  1. |--> ~/quicklisp/local-projects/
#  2. |--> ~/common-lisp/
#  3. |--> ~/.local/share/common-lisp/source/

# Save development common-lisp libraries/packages here
common_lisp=/home/logoraz/common-lisp/
if [ ! -d $common_lisp ]; then
    echo "~/common-lisp/ does not exist - creating..."
    mkdir $common_lisp
fi

# Save persistent common-lisp libraries/packages here
common_lisp_local=/home/logoraz/.local/share/common-lisp/
if [ ! -d $common_lisp_local ]; then
    echo "~/.local/share/common-lisp/ does not exist - creating..."
    mkdir $common_lisp_local
fi
common_lisp_source=/home/logoraz/.local/share/common-lisp/source/
if [ ! -d $common_lisp_source ]; then
    echo "~/.local/share/common-lisp/source/ does not exist - creating..."
    mkdir $common_lisp_source
fi


# SBCL Init file
ln -s ~/repos/guix-craft/config/common-lisp/dot-sbclrc.lisp \
   ~/.sbclrc

# CLASP-CL Init file?


# Quicklisp
# setting up so you don't have to re-download everytime
quicklisp=/home/logoraz/quicklisp/
if [ ! -d $quicklisp ]; then
    echo "/home/logoraz/quicklisp/ does not exist - creating..."
    mkdir ~/quicklisp/
fi

if [ ! -d $quicklisp/install ]; then
    echo "/home/logoraz/quicklisp/install/ does not exist - creating.."
    mkdir $quicklisp/install/
    cp ~/repos/guix-craft/config/common-lisp/quicklisp/quicklisp.lisp \
       ~/quicklisp/install/quicklisp.lisp
    cp ~/repos/guix-craft/config/common-lisp/quicklisp/quicklisp.asc \
       ~/quicklisp/install/quicklisp.asc
fi
