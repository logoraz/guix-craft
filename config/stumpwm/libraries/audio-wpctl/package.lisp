;;;; package.lisp

(defpackage #:audio-wpctl
  (:use #:cl #:stumpwm)
  (:export *caps-lock-behavior*
           *custom-setxkb-options*
           *run-xmodmap*
           keyboard-layout-list))
