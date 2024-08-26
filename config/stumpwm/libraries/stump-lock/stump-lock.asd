;;;; stump-lock.asd

(defsystem "stump-lock"
  :serial t
  :description "Screen locker in StumpWM"
  :author "Florian Margaine, Erik P Almaraz"
  :license "GPLv3"
  :version "0.1.1"
  :depends-on ("stumpwm")
  :components ((:file "package")
               (:file "lock")))
