;;;; utilities.lisp --> Utilities for StumpWM

;;; Author:
;;; Erik P Almaraz

;;; License:
;;; GPLv3

;;; Commentary:
;;;

;;; References:
;;;

(in-package :stumpwm)

;; Pipewire/Wirepluber Audio Controls for StumpWM
;; TODO: Refactor -> move to modeline file.
(add-to-load-path #p"~/.config/stumpwm/libraries/wpctl/")
(load-module "wpctl")
(setf wpctl:*modeline-fmt* "α %v")
(setf wpctl:*wpctl-path* "/home/logoraz/.guix-home/profile/bin/wpctl")
(setf wpctl:*mixer-command* "playerctl")

;; Simple Bluetooth Controls for StumpWM
;; TODO: Add modeline display
(add-to-load-path #p"~/.config/stumpwm/libraries/bluetooth/")
(load-module "bluetooth")

;;; Stumpwm-contrib packages not available in Guix
;; `end-session' - Provides session control commands, i.e. shutdown, restart,
;; and logoff for StumpWM.
(add-to-load-path #p"~/.config/stumpwm/libraries/end-session/")
(load-module "end-session")
;; Use loginctl instead of the default systemctl
(setf end-session:*end-session-command* "loginctl")

;;; Screenshots via Common Lisp -> removed scrot!
;;; Modified stumpwwm-contrib package screenshot
(add-to-load-path #p"~/.config/stumpwm/libraries/screenshot/")
(load-module "screenshot")
