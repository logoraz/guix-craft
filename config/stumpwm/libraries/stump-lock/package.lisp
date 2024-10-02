;;;; package.lisp -> stump-lock

;;; Author:
;;; Erik P Almaraz

;;; License:
;;; GPLv3

;;; Commentary:
;;;

(uiop:define-package #:stump-lock
  (:use #:cl :stumpwm)
  (:export #:*password*))
