#!/bin/sh

# Launch xterm in the background
/home/logoraz/.guix-home/profile/bin/xterm &

# Sleep long enough to get the window open, and set transparency
sleep 0.30s
transset -a 0.80
