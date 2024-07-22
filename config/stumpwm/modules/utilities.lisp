;;;; utilities.lisp --> Utilities for StumpWM

;;; Commentary:

;;; References:

(in-package :stumpwm)

;;; Screenshots via Common Lisp -> removed scrot!
(load-module "screenshot")

;;; Notifications
;; This module does not work -> crashes StumpWM when toggling notify server...
;;(load-module "notify")
;;(notify:notify-server-toggle)
