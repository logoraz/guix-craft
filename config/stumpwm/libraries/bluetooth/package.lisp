;;;; package.lisp --> bluetooth

(defpackage #:bluetooth
  (:use #:cl :stumpwm :cl-ppcre)
  (:export #:bluetooth-message-command
           #:bluetooth-turn-off-command
           #:bluetooth-connect-device
           #:*bluetooth-command*))
