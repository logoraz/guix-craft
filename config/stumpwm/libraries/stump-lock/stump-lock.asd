;;;; stump-lock.asd

;;; Author:
;;; Erik P Almaraz

;;; License:
;;; GPLv3

;;; Commentary:
;;;

(defsystem "stump-lock"
  :serial t
  :description "Screen locker in StumpWM"
  :author "Erik P Almaraz, Florian Margaine"
  :license "GPLv3"
  :version "0.1.1"
  :depends-on ("stumpwm")
  :components ((:file "package")
               (:file "lock")))
