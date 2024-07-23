;;;; audio-wpctl.asd
(asdf:compute-source-registry)
(declaim (optimize (speed 0) (debug 3) (safety 3)))

(asdf:defsystem #:audio-wpctl
  :description "Wireplumbler Audio Controls for StumpWM"
  :author "Erik P. Almaraz"
  :license "GPLv3"
  :serial t
  :depends-on (#:stumpwm "parse-float")
  :components ((:file "package")
               (:file "audio-wpctl")))
