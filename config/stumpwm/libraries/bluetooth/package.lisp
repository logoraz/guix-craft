;;;; package.lisp

(defpackage #:bluetooth
  (:use #:cl :stumpwm)
  (:export #:bluetooth-turn-on
           #:bluetooth-turn-off
           #:bluetooth-connect))
