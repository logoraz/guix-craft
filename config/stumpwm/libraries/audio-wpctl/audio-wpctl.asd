;;;; audio-wpctl.asd
(asdf:compute-source-registry)
(declaim (optimize (speed 0) (debug 3) (safety 3)))

(asdf:defsystem #:audio-wpctl
  :serial t
  :description "Wireplumbler Audio Controls for StumpWM"
  :author "Erik P. Almaraz"
  :license "GPLv3"
  :depends-on (#:stumpwm)
  :components ((:file "package")
               (:file "audio-wpctl")))
