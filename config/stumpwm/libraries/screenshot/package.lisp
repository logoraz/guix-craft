;;;; package.lisp --> screenshot

;;; Author:
;;; Erik P Almaraz

;;; License:
;;; GPLv3

;;; Commentary:
;;;

(uiop:define-package #:screenshot
  (:use #:cl :stumpwm)
  (:local-nicknames (:xl :xlib)
                    (:lt :local-time))
  (:export #:screenshot
           #:screenshot-window
           #:screenshot-area))
