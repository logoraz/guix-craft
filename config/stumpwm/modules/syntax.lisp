;;;; syntax.lisp --> StumpWM Macro's, Helpers, & Commands

;;; Author:
;;; Erik P Almaraz

;;; License:
;;; GPLv3

;;; Commentary:
;;;

;;; References:
;;;

(in-package :stumpwm)

;;; StumpWM Commands --> keybindings set in keybindings.lisp file

;; (defcommand firefox () ()
;;   "Run or raise Firefox."
;;   (sb-thread:make-thread (lambda () (run-or-raise "firefox" '(:class "Firefox") t nil))))

(defcommand delete-window-and-frame () ()
  "Delete the current frame with its window."
  (delete-window)
  (remove-split))

(defcommand hsplit-and-focus () ()
  "Create a new frame on the right and focus it."
  (hsplit)
  (move-focus :right))

(defcommand vsplit-and-focus () ()
  "Create a new frame below and move focus to it."
  (vsplit)
  (move-focus :down))

(defcommand term (&optional program) ()
  "Invoke a terminal, possibly with a @arg{program}."
  (sb-thread:make-thread
   (lambda ()
     (run-shell-command (if program
                            (format nil "kitty ~A" program)
                            "kitty")))))

;;; Common Lisp Servers (Slynk & Swank)
;; Slynk (preferred --> stumpwm+slynk package
(defcommand slynk-start-server () ()
  "Start a slynk server for sly."
  (require :slynk)
  (sb-thread:make-thread
   (lambda () (slynk:create-server :port 4005 :dont-close t)))
  (echo-string (current-screen) "Starting slynk."))

(defcommand slynk-stop-server () ()
  "Stop current slynk server for sly."
  (sb-thread:make-thread
   (lambda () (slynk:stop-server 4005)))
  (echo-string (current-screen "Closing slynk.")))

;; Kept for archive purposes
;; (defcommand slynk (port) ((:string "Port number: "))
;;   (sb-thread:make-thread
;;    (lambda ()
;;      (slynk:create-server :port (parse-integer port) :dont-close t))
;;    :name "Start Slynk server process."))


;;; micros (cl-micros) --> ~/common-lisp
(defcommand micros-start-server () ()
  "Start a swank server."
  (sb-thread:make-thread
   (lambda () (micros:create-server :port 4005 :dont-close t)))
  (echo-string (current-screen) "Starting micros/swank."))

(defcommand micros-stop-server () ()
  "Stop current swank server."
  (sb-thread:make-thread
   (lambda () (micros:stop-server 4005)))
  (echo-string (current-string) "Closing micros/swank."))
