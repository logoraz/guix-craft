#!/bin/sh -e
#
# open file in application based on file extension

case $1 in
    *.mp3|*.flac|*.wav)
        # mpv --no-video "$1"
        mpv "$1"
    ;;

    *.mp4|*.mkv|*.webm)
        mpv "$1"
    ;;

    *.png|*.gif|*.jpg|*.jpe|*.jpeg)
        emacs "$1"
    ;;

    *.epub|*.pdf)
        zathura --plugins-dir=$HOME/.guix-home/profile/lib/zathura "$1"
    ;;

    *.html)
        qutebrowser "$1"
    ;;

    # all other files
    *)
        "${EDITOR:=emacs}" "$1"
    ;;
esac
