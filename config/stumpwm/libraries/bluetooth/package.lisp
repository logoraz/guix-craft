;;;; package.lisp --> bluetooth

(defpackage #:bluetooth
  (:use #:cl)
  (:import-from :cl-ppcre
                #:split)
  (:import-from :stumpwm
                #:message
                #:defcommand
                #:select-from-menu
                #:current-screen
                #:run-shell-command)
  (:export #:bluetooth-message-command
           #:bluetooth-turn-off-command
           #:bluetooth-connect-device
           #:*bluetooth-command*))
