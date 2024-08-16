;;;; syntax.lisp --> StumpWM Macro's, Helpers, & Commands

;;; Commentary:

;;; References:

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

;; (defcommand term (&optional program) ()
;;   "Invoke a terminal, possibly with a @arg{program}."
;;   (sb-thread:make-thread
;;    (lambda ()
;;      (run-shell-command (if program
;;                             (format nil "kitty ~A" program)
;;                             "kitty")))))


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
