;;;; misc.lisp

(uiop:define-package :config/misc
  (:use :cl :lem))
(in-package :config/misc)

;; Load Theme
;; (load-theme "decaf") ; default

;; Logs on the terminal output:
(log:config :info)

;;; Commands
(define-command open-init-file () ()
  ;; @sasanidas
  (lem:find-file
   (merge-pathnames "init.lisp" (lem-home))))

