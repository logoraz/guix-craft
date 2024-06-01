#!/bin/sh

guix pull --channels=$HOME/.config/guix/channels.scm
guix describe --format=channels > ~/.config/guix/channels.scm
