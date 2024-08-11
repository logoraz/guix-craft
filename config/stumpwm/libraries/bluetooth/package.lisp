;;;; package.lisp --> bluetooth

(defpackage #:bluetooth
  (:use #:cl :stumpwm)
  (:local-nicknames (:re :ppcre))
  (:export #:bluetooth-message-command
           #:bluetooth-turn-off-command
           #:bluetooth-connect-device
           #:*bluetooth-command*))
