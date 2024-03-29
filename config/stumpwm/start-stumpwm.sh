#!/bin/sh
# Setup:
# ln -f ~/repos/guix-craft/config/stumpwm/start-stumpwm.sh \
#       ~/.config/stumpwm/start-stumpwm.sh

#
# Xorg Settings
#

# Set fallback mouse pointer/cursor themes
export XCURSOR_PATH=~/.icons/:/run/current-system/profile/share/icons/
xsetroot -xcf ~/.icons/XCursor-Pro-Dark/cursors/left_ptr 22

# Turn off system bell & screen-saver control
xset b off
xset s off

#
# UI Settings
#

# Set Wallpaper
feh --bg-scale  ~/desktop/wallpapers/sunset-mountain.jpg

# Enable screen compositing
picom &

# Enable screen locking on suspend
xss-lock -- slock &

#
# Start StumpWM
#

# See: https://mail.gnu.org/archive/html/bug-guix/2023-04/msg00227.html
exec sbcl --no-userinit --non-interactive --eval '(require :asdf)' \
     --eval '(asdf:load-system "stumpwm")' --eval '(stumpwm:stumpwm)'

