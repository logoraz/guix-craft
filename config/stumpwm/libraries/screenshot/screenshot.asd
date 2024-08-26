;;;; screenshot.asd

(defsystem "screenshot"
  :serial t
  :description "Takes screenshots and stores them as png files"
  :author "Michael Filonenko, Erik P Almaraz"
  :license "GPLv3"
  :depends-on ("stumpwm" "clx" "zpng" "local-time")
  :components ((:file "package")
               (:file "screenshot")))
