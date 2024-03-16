#!/bin/sh
# Setup:
# ln -f ~/repos/guix-craft/config/stumpwm/start-stumpwm.sh \
#       ~/.config/stumpwm/start-stumpwm.sh

# Disable access control for the current user
#xhost +SI:localuser:$USER

# Disable touchpad/trackpad
# https://packages.guix.gnu.org/packages/xinput/1.6.3/
# https://askubuntu.com/questions/919495/how-to-disable-touchpad-completely-on-boot#answer-919496
# xinput --set-prop 14 "Device Enabled" 0
# xinput --set-prop 14 "Device Enabled" 1

# Set fallback pointer
xsetroot -cursor_name left_ptr

# Set sound to ALSA
export SDL_AUDIODRIVER=alsa

# Fix scrolling on some GTK3 applications
export GDK_CORE_DEVICE_EVENTS=1

# Enable screen compositing
picom &

# Turn off the system bell
# Doesn't seem to bee working...
xset -b

# Enable screen locking on suspend
xss-lock -- slock &

# xrandr

# See: https://mail.gnu.org/archive/html/bug-guix/2023-04/msg00227.html
exec sbcl --no-userinit --non-interactive --eval '(require :asdf)' \
     --eval '(asdf:load-system "stumpwm")' --eval '(stumpwm:stumpwm)'
