;;;; utilities.lisp --> Utilities for StumpWM

;;; Commentary:

;;; References:

(in-package :stumpwm)

;;; Define commands to create slynk server -> no need to run all the time.
(require :slynk)

(defcommand sly-start-server () ()
  "Start a slynk server for sly."
  (sb-thread:make-thread (lambda () (slynk:create-server :dont-close t))))

(defcommand sly-stop-server () ()
  "Stop current slynk server for sly."
  (sb-thread:make-thread (lambda () (slynk:stop-server 4005))))

;;; Screenshots via Common Lisp -> removed scrot!
(load-module "screenshot")

;;; Notifications
;; This module does not work -> crashes StumpWM when toggling notify server...
;;(load-module "notify")
;;(notify:notify-server-toggle)
