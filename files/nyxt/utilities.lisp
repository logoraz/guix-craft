;;;; Nyxt Utilities Module

;;; Commentary:

;;; References:
;;; 1. TBD
;;; 2. TBD
;;;

;;; TODO: 1. Look into how Emacs enables transparency in Xorg and see if we can't apply such a
;;;       feature to Nyxt! Currently using Xorg's transset (hack)!
;;;       2. Look into customizing fonts...
;;;       3. Apply theme colors & fonts to Nyxt's internal editor...
;;;          Would like to work on this feature to help get Nyxt to where Emacs is, i.e. first
;;;          implement syntax highlighting!
;;;       4. Customize `edit-file' command/interface
;;;       5. TBD
;;;       6. TBD

(in-package #:nyxt)

(defvar *micros-port* 4006
  "default micros server port for Nyxt.")

(define-command start-micros (&optional (micros-port *micros-port*))
  "Start a micros server enabling connecting to Lem."
  (micros:create-server :port micros-port :dont-close t)
  (echo "Micros server started at port ~a" micros-port))

(define-command stop-micros (&optional (micros-port *micros-port*))
  "Stop current micros server."
  (micros:stop-server micros-port)
  (echo "Closing micros/swank server at port ~a" micros-port))
