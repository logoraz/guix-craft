#!/bin/sh
# Deploy Emacs Config

# TODO - Once I learn guile well enough, will use Guix as a deploy agent for this...

# Check if Emacs directory exists else create
emacs=~/.config/emacs/
if [ ! -d $emacs ]; then
    echo "~/.config/emacs/ does not exist - creating..."
    mkdir ~/.config/emacs/
fi

# Initialization Files
ln -f ~/repos/guix-craft/config/emacs/early-init.el \
   ~/.config/emacs/early-init.el

ln -f ~/repos/guix-craft/config/emacs/init.el \
   ~/.config/emacs/init.el

# Emacs Lisp Language Extensions
elisp=~/.config/emacs/elisp/
if [ ! -d $elisp ]; then
    echo "~/.config/emacs/elisp/ does not exist - creating..."
    mkdir ~/.config/emacs/elisp/
fi

ln -f ~/repos/guix-craft/config/emacs/elisp/raz-subrx.el \
   ~/.config/emacs/elisp/raz-subrx.el

# Custom Themes
themes=~/.config/emacs/themes/
if [ ! -d $themes ]; then
    echo "~/.config/emacs/themes/ does not exist - creating..."
    mkdir ~/.config/emacs/themes/
fi

ln -f ~/repos/guix-craft/config/emacs/themes/nord-theme.el \
   ~/.config/emacs/themes/nord-theme.el

ln -f ~/repos/guix-craft/config/emacs/themes/kanagawa-theme.el \
   ~/.config/emacs/themes/kanagawa-theme.el

# Modules
modules=~/.config/emacs/modules/
if [ ! -d $modules ]; then
    echo "~/.config/emacs/modules/ does not exist - creating..."
    mkdir ~/.config/emacs/modules/
fi

# Emacs Base Configuration
ln -f ~/repos/guix-craft/config/emacs/modules/raz-base.el \
   ~/.config/emacs/modules/raz-base.el

# Completions System
ln -f ~/repos/guix-craft/config/emacs/modules/raz-completions-mct.el \
   ~/.config/emacs/modules/raz-completions-mct.el

# Common Lisp IDE
ln -f ~/repos/guix-craft/config/emacs/modules/raz-lisp-ide.el \
   ~/.config/emacs/modules/raz-lisp-ide.el

# Guile Scheme IDE
ln -f ~/repos/guix-craft/config/emacs/modules/raz-guile-ide.el \
   ~/.config/emacs/modules/raz-guile-ide.el

# Note taking system
ln -f ~/repos/guix-craft/config/emacs/modules/raz-denote.el \
   ~/.config/emacs/modules/raz-denote.el

# Media Player
ln -f ~/repos/guix-craft/config/emacs/modules/raz-media.el \
   ~/.config/emacs/modules/raz-media.el

# Org
ln -f ~/repos/guix-craft/config/emacs/modules/raz-org.el \
   ~/.config/emacs/modules/raz-org.el

# IRC
ln -f ~/repos/guix-craft/config/emacs/modules/raz-erc.el \
   ~/.config/emacs/modules/raz-erc.el

# Mail
ln -f ~/repos/guix-craft/config/emacs/modules/raz-mu4e.el \
   ~/.config/emacs/modules/raz-mu4e.el

# Nyxt Browser
ln -f ~/repos/guix-craft/config/emacs/modules/raz-nyxt.el \
   ~/.config/emacs/modules/raz-nyxt.el
