;;;; screenshot.asd

(asdf:defsystem #:screenshot
  :serial t
  :description "Takes screenshots and stores them as png files"
  :author "Michael Filonenko & Erik Almaraz"
  :license "GPLv3"
  :depends-on (#:stumpwm :clx :zpng :local-time)
  :components ((:file "package")
               (:file "screenshot")))
