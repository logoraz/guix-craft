;;;; utilities.lisp --> Utilities for StumpWM

;;; Commentary:

;;; References:

(in-package :stumpwm)


(require :slynk)
;;; Define commands to create slynk server -> no need to run all the time.
(defcommand sly-start-server () ()
  "Start a slynk server for sly."
  (sb-thread:make-thread (lambda () (slynk:create-server :dont-close t))))

(defcommand sly-stop-server () ()
  "Stop current slynk server for sly."
  (sb-thread:make-thread (lambda () (slynk:stop-server 4005))))

;; Add to keybindings.lisp
;; (define-key key-map (kbd "y") "sly-start-server")
;; (define-key key-map (kbd "z") "sly-stop-server")

;;; Screenshots via Common Lisp -> removed scrot!
(load-module "screenshot")

;;; Notifications
;; This module does not work -> crashes StumpWM when toggling notify server...
;;(load-module "notify")
;;(notify:notify-server-toggle)
