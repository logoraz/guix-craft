;;;; utilities.lisp --> Utilities for StumpWM

;;; Commentary:

;;; References:

(in-package :stumpwm)

;;; Define commands to create slynk server -> no need to run all the time.
;; (require :slynk)
;; (defcommand sly-start-server () ()
;;   "Start a slynk server for sly."
;;   (sb-thread:make-thread
;;    (lambda () (slynk:create-server :port 4005 :dont-close t))))

;; (defcommand sly-stop-server () ()
;;   "Stop current slynk server for sly."
;;   (sb-thread:make-thread
;;    (lambda () (slynk:stop-server 4005))))

;; Add to keybindings.lisp
;; (define-key key-map (kbd "y") "sly-start-server")
;; (define-key key-map (kbd "z") "sly-stop-server")

;;; Screenshots via Common Lisp -> removed scrot!
;;; Modified stumpwwm-contrib package screenshot
(add-to-load-path #p"~/.local/share/common-lisp/stumpwm-contrib/screenshot/")
(load-module "screenshot")
