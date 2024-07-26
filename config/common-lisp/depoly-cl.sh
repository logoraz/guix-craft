#!/bin/sh
# Deploy Common Lisp Environment

# TODO - Once I learn guile well enough, will use Guix as a deploy agent for this...

# See Advanced Dependencies Management
# |-> ;; https://lispcookbook.github.io/cl-cookbook/getting-started.html
# You can drop Common Lisp projects into any of these folders:
#  1. |--> ~/common-lisp/
#  2. |--> ~/.local/share/common-lisp/source/
#  3. |--> ~/quicklisp/local-projects/
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


# CLPM & ASDF Registry Setup
common_lisp_config=/home/logoraz/.config/common-lisp/
if [ ! -d $common_lisp_config ]; then
    echo "~/.config/common-lisp/ does not exist - creating directory"
    mkdir $common_lisp_config
fi

asdf_reg=/home/logoraz/.config/common-lisp/source-registry.conf.d
if [ ! -d $asdf_reg ] && [ ! -L $asdf_reg ]; then
    echo "~/.config/common-lisp/source-registry.conf.d/ does not exist - creating link"
    ln -s ~/repos/guix-craft/config/common-lisp/source-registry.conf.d \
       $asdf_reg
fi

clpm=/home/logoraz/.config/clpm
if [ ! -d $clpm ] && [ ! -L $clpm ]; then
    echo "~/.config/clpm/ does not exist - creating link"
    ln -s ~/repos/guix-craft/config/common-lisp/clpm \
       $clpm
fi
