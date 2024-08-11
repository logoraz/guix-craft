;;;; package.lisp --> screenshot

(defpackage #:screenshot
  (:use #:cl :stumpwm)
  (:local-nicknames (:xl :xlib)
                    (:lt :local-time))
  (:export #:screenshot
           #:screenshot-window
           #:screenshot-area))
