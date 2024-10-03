;;;; package.lisp --> swm-screenshot

;;; Author:
;;; Erik P Almaraz

;;; License:
;;; GPLv3

;;; Commentary:
;;;

(uiop:define-package #:swm-screenshot
  (:use #:cl :stumpwm)
  (:local-nicknames (:xl :xlib)
                    (:lt :local-time))
  (:export #:screenshot
           #:screenshot-window
           #:screenshot-area))
