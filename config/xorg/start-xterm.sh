#!/bin/sh
# Setup:
# ln -f ~/repos/guix-craft/config/xorg/xterm.sh \
#       ~/.config/xorg/xterm.sh

# Launch xterm in the background
/run/current-system/profile/bin/xterm &

# Sleep long enough to get the window open, and set transparency
sleep .15s
transset -a 0.80
