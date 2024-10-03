;;;; package.lisp --> swm-bluetooth

;;; Author:
;;; Erik P Almaraz

;;; License:
;;; GPLv3

;;; Commentary:
;;;

(uiop:define-package #:swm-bluetooth
  (:use #:cl :stumpwm)
  (:local-nicknames (:re :ppcre))
  (:export #:bluetooth-message-command
           #:bluetooth-turn-off-command
           #:bluetooth-connect-device
           #:*bluetooth-command*))
