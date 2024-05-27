#!/bin/sh
# Setup:

# Launch xterm in the background
~/.guix-home/profile/bin/nyxt &

# Sleep long enough to get the window open, and set transparency
sleep 2.15s
transset -a 0.85
