;;;; -*- mode: stumpwm-mode -*-
;;;; Stump WM Audio (wpctl) Module
;;;
;;; Setup:
;;; ln -f ~/repos/guix-craft/config/stumpwm/modules/audio-wpctl.lisp \
;;;       ~/.config/stumpwm/audio-wpctl.lisp
;;;
;;; Ref: https://github.com/Junker/stumpwm-wpctl


(defparameter *step* 5)
(defparameter *wpctl-path* "/home/logoraz/.guix-home/profile/bin/wpctl")
(defparameter *default-sink-id* "@DEFAULT_AUDIO_SINK@")
(defparameter *default-source-id* "@DEFAULT_AUDIO_SOURCE@")

(defun run (args &optional (wait-output nil))
  (if wait-output
      (with-output-to-string (s)
        (sb-ext:run-program *wpctl-path* args :wait t :output s))
      (sb-ext:run-program *wpctl-path* args :wait nil)))

(defun volume-up (device-id step)
  (run (list "set-volume" device-id (format nil "~D%+" step))))

(defun volume-down (device-id step)
  (run (list "set-volume" device-id (format nil "~D%-" step))))

(defun toggle-mute (device-id)
  (run (list "set-mute" device-id "toggle")))

(defcommand wpctl-volume-up () ()
  "Increase the volume by N points"
  (volume-up *default-sink-id* *step*))

(defcommand wpctl-volume-down () ()
  "Decrease the volume by N points"
  (volume-down *default-sink-id* *step*))

(defcommand wpctl-toggle-mute () ()
  "Toggle Mute"
  (toggle-mute *default-sink-id*))
