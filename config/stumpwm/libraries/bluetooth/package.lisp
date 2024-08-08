;;;; package.lisp --> bluetooth

(defpackage #:bluetooth
  (:import-from :cl-ppcre
                #:split)
  (:import-from :stumpwm
                #:message
                #:defcommand
                #:select-from-menu
                #:current-screen
                #:run-shell-command)
  (:use #:cl)
  (:export #:bluetooth-message-command
           #:bluetooth-turn-off-command
           #:bluetooth-connect-device
           #:*bluetooth-command*))
