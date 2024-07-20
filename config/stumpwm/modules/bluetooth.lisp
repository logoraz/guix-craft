;;;; StumpWM Bluetooth Module

;;; Commentary:

;;; References:
;;; 1. https://config.phundrak.com/stumpwm#bluetooth

(in-package :stumpwm)

;;(require cl-pprce)
(load-module "cl-ppcre")

(defvar *bluetooth-command* "bluetoothctl"
  "Base command for interacting with bluetooth.")

(defun bluetooth-message (&rest message)
  (message (format nil
                   "^2Bluetooth:^7 ~{~A~^ ~}"
                   message)))

(defun bluetooth-make-command (&rest args)
  (format nil
          "~a ~{~A~^ ~}"
          *bluetooth-command*
          args))

(defmacro bluetooth-command (&rest args)
  `(run-shell-command (bluetooth-make-command ,@args) t))

(defmacro bluetooth-message-command (&rest args)
  `(bluetooth-message (bluetooth-command ,@args)))

(defcommand bluetooth-turn-on () ()
  "Turn on bluetooth."
  (bluetooth-message-command "power" "on"))

(defcommand bluetooth-turn-off () ()
  "Turn off bluetooth."
  (bluetooth-message-command "power" "off"))

(defstruct (bluetooth-device
             (:constructor
              make-bluetooth-device (&key (address "")
                                          (name nil)))
             (:constructor
              make-bluetooth-device-from-command
              (&key (raw-name "")
               &aux (address (cadr (cl-ppcre:split " " raw-name)))
                    (full-name (format nil "~{~A~^ ~}" (cddr (cl-ppcre:split " " raw-name)))))))
  address
  (full-name (progn
                 (format nil "~{~A~^ ~}" name))))

(defun bluetooth-get-devices ()
  (let ((literal-devices (bluetooth-command "devices")))
    (mapcar (lambda (device)
              (make-bluetooth-device-from-command :raw-name device))
     (cl-ppcre:split "\\n" literal-devices))))

(defun bluetooth-connect-device (device)
  (progn
    (bluetooth-turn-on)
    (cond ((bluetooth-device-p device) ;; it is a bluetooth-device structure
           (bluetooth-message-command "connect"
                                      (bluetooth-device-address device)))
          ((stringp device)            ;; assume it is a MAC address
           (bluetooth-message-command "connect" device))
          (t (message (format nil "Cannot work with device ~a" device))))))

(defcommand bluetooth-connect () ()
  (sb-thread:make-thread
   (lambda ()
    (let* ((devices (bluetooth-get-devices))
           (choice  (cadr (stumpwm:select-from-menu
                           (stumpwm:current-screen)
                           (mapcar (lambda (device)
                                     `(,(bluetooth-device-full-name device) ,device))
                                   devices)))))
      (bluetooth-connect-device choice)))))

(defvar *bluetooth-keymap*
  (let ((key-map (make-sparse-keymap)))
    (define-key key-map (kbd "c") "bluetooth-connect")
    (define-key key-map (kbd "o") "bluetooth-turn-on")
    (define-key key-map (kbd "O") "bluetooth-turn-off")
    m))

(define-key *root-map* (kbd "B") '*bluetooth-keymap*)
