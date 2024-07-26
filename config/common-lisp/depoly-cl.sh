#!/bin/sh
# Deploy Common Lisp Environment

# TODO - Once I learn guile well enough, will use Guix as a deploy agent for this...

# ASDF search locations for CL packages.
#  1. |--> ~/common-lisp/
#  2. |--> ~/.local/share/common-lisp/source/
# Save development common-lisp libraries/packages here

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


# ASDF Registry Setup
# Ref: https://www.sbcl.org/manual/asdf.html#Configuring-ASDF
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
