;;;; bluetooth.asd

(asdf:defsystem #:bluetooth
  :description "Provides simple interface to connect to Bluetooth"
  :author "Erik P. Almaraz"
  :license "GPLv3"
  :serial t
  :depends-on (#:stumpwm #:cl-ppcre)
  :components ((:file "package")
               (:file "bluetooth")))
