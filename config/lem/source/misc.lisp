;;;; misc.lisp

(defpackage :config/misc
  (:use :cl :lem))

(in-package :config/misc)

;;; TODO
;; Load Theme
;; (load-theme "decaf") ; default

;; Logs on the terminal output:
;; (log:config :info)

;;; Commands
(define-command open-init-file () ()
  ;; @sasanidas
  (lem:find-file
   (merge-pathnames "init.lisp" (lem-home))))

;;; Exploratory
;; Version Control
(define-command start-legit () ()
  "Lem Command to start legit in thread."
  (bt:make-thread
   (lambda ()
     (require :lem/legit)
     ;; for some reason loading legit unsets keybindings and theme...
     (load-theme "decaf")
     (custom-keybindings))))

