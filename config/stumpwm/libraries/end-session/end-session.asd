;;;; end-session.asd

(defsystem "end-session"
  :description "StumpWM UI commands for shutdown, restart, and logoff."
  :author "Stuart Dilts, Erik P Almaraz"
  :license "GPLv3"
  :version "0.1.1"
  :depends-on ("stumpwm")
  :serial t
  :components ((:file "package")
               (:file "end-session")))
