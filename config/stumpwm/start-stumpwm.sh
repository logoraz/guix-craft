#!/bin/sh
# Setup:
# ln -f ~/repos/guix-craft/config/stumpwm/start-stumpwm.sh \
#       ~/.config/stumpwm/start-stumpwm.sh

# exec xmodmap ~/.xmodmap
xrdb -load ~/.Xresources

# Set fallback pointer
xsetroot -cursor_name left_ptr

# Turn off system bell & screen-saver control
xset b off
xset s off

# Fix scrolling on some GTK3 applications
export GDK_CORE_DEVICE_EVENTS=1

# Disable touchpad/trackpad
# https://packages.guix.gnu.org/packages/xinput/1.6.3/
# https://askubuntu.com/questions/919495/how-to-disable-touchpad-completely-on-boot#answer-919496
# exec xinput --set-prop 14 "Device Enabled" 0
# exec xinput --set-prop 14 "Device Enabled" 1

# Enable screen locking on suspend
xss-lock -- slock &

# xrandr settings here...

# Enable screen compositing
picom &

# See: https://mail.gnu.org/archive/html/bug-guix/2023-04/msg00227.html
exec sbcl --no-userinit --non-interactive --eval '(require :asdf)' \
     --eval '(asdf:load-system "stumpwm")' --eval '(stumpwm:stumpwm)'
