;;;; audio-wpctl --> Wireplumber Audio Controls for StumpWM

;;; Commentary:

;;; References:
;;;  1. https://github.com/Junker/stumpwm-wpctl
;;;  2. https://github.com/stumpwm/stumpwm-contrib/tree/master

(in-package :stumpwm)

(load-module "parse-float")

(defvar *wpctl-modeline-fmt* "AUD: %v" ;"%b (%v)"
  "Default value for displaying wpctl information on the modeline.")

(add-screen-mode-line-formatter #\P 'modeline)
;; (add-screen-mode-line-formatter #\M 'source-modeline)

(defparameter *step* 5)

(defparameter *modeline-fmt* *wpctl-modeline-fmt*
  "The default value for displaying wpctl information on the modeline")

(defparameter *source-modeline-fmt* *wpctl-modeline-fmt*
  "The default value for displaying wpctl source information on the modeline")

(defparameter *formatters-alist*
  '((#\b  ml-bar)
    (#\v  ml-volume)))

(defparameter *wpctl-path* "/home/logoraz/.guix-home/profile/bin/wpctl")
;; FIXME - this should be something customizable by the user
;; change pavucontrol -> playerctl
(defparameter *mixer-command* "playerctl")

(defparameter *default-sink-id* "@DEFAULT_AUDIO_SINK@")
(defparameter *default-source-id* "@DEFAULT_AUDIO_SOURCE@")

(defvar *volume-regex* (ppcre:create-scanner "Volume: (\\d+\\.\\d+)"))
(defvar *mute-regex* (ppcre:create-scanner "Volume: \\d+\\.\\d+ \\[MUTED\\]"))

(defun run (args &optional (wait-output nil))
  (if wait-output
      (with-output-to-string (s)
        (sb-ext:run-program *wpctl-path* args :wait t :output s))
      (sb-ext:run-program *wpctl-path* args :wait nil)))

(defun volume-up (device-id step)
  (run (list "set-volume" device-id (format nil "~D%+" step))))

(defun volume-down (device-id step)
  (run (list "set-volume" device-id (format nil "~D%-" step))))

(defun set-volume (device-id value)
  (run (list "set-volume" device-id (format nil "~D%" value))))

(defun get-volume (device-id)
  (truncate (* 100 (parse-float:parse-float
                    (aref
                     (nth-value
                      1
                      (ppcre:scan-to-strings
                       *volume-regex*
                       (run (list "get-volume" device-id) t)))
                     0)))))

(defun get-mute (device-id)
  (and (ppcre:scan
        *mute-regex*
        (run (list "get-volume" device-id) t))
       t))

(defun unmute (device-id)
  (run (list "set-mute" device-id "0")))

(defun mute (device-id)
  (run (list "set-mute" device-id "1")))

(defun toggle-mute (device-id)
  (run (list "set-mute" device-id "toggle")))

(defun open-mixer ()
  (run-shell-command *mixer-command*))

;;; Modeline Logic
(defun ml-bar (volume muted)
  (concat "\["
          (stumpwm:bar (if muted 0 (min 100 volume)) 5 #\X #\=)
          "\]"))

(defun ml-volume (volume muted)
  (if muted "MUT" (format nil "~a\%" volume)))


(defun modeline (ml)
  (declare (ignore ml))
  (let ((ml-str (format-expand
                 *formatters-alist*
                 *modeline-fmt*
                 (get-volume *default-sink-id*) (get-mute *default-sink-id*))))
    (if (fboundp 'stumpwm::format-with-on-click-id) ; check in case of old stumpwm version
        (format-with-on-click-id ml-str :ml-wpctl-on-click nil)
        ml-str)))

(defun source-modeline (ml)
  (declare (ignore ml))
  (let ((ml-str (format-expand
                 *formatters-alist*
                 *source-modeline-fmt*
                 (get-volume *default-source-id*) (get-mute *default-source-id*))))
    (if (fboundp 'stumpwm::format-with-on-click-id) ;check in case of old stumpwm version
        (format-with-on-click-id ml-str :ml-wpctl-source-on-click nil)
        ml-str)))

(defun ml-on-click (code id &rest rest)
  (declare (ignore rest))
  (declare (ignore id))
  (let ((button (stumpwm::decode-button-code code)))
    (case button
      ((:left-button)
       (toggle-mute *default-sink-id*))
      ((:right-button)
       (open-mixer))
      ((:wheel-up)
       (volume-up *default-sink-id* *step*))
      ((:wheel-down)
       (volume-down *default-sink-id* *step*))))
  (stumpwm::update-all-mode-lines))

(defun source-ml-on-click (code id &rest rest)
  (declare (ignore rest))
  (declare (ignore id))
  (let ((button (stumpwm::decode-button-code code)))
    (case button
      ((:left-button)
       (toggle-mute *default-source-id*))
      ((:right-button)
       (open-mixer))
      ((:wheel-up)
       (volume-up *default-source-id* *step*))
      ((:wheel-down)
       (volume-down *default-source-id* *step*))))
  (stumpwm::update-all-mode-lines))

(when (fboundp 'stumpwm::register-ml-on-click-id) ; check in case of old stumpwm version
  (register-ml-on-click-id :ml-wpctl-on-click #'ml-on-click)
  (register-ml-on-click-id :ml-wpctl-source-on-click #'source-ml-on-click))

;;; StumpWM Interface Definitions
(stumpwm:defcommand wpctl-volume-up () ()
  "Increase the volume by N points"
  (volume-up *default-sink-id* *step*))

(defcommand wpctl-volume-down () ()
  "Decrease the volume by N points"
  (volume-down *default-sink-id* *step*))

(defcommand wpctl-mute () ()
  "Mute"
  (mute *default-sink-id*))

(defcommand wpctl-unmute () ()
  "Unmute"
  (unmute *default-sink-id*))

(defcommand wpctl-toggle-mute () ()
  "Toggle Mute"
  (toggle-mute *default-sink-id*))

(defcommand wpctl-set-volume (value) ((:string "Volume percentage:"))
  "Set volume"
  (set-volume *default-sink-id* value))

(defcommand wpctl-source-volume-up () ()
  "Increase the volume by N points"
  (volume-up *default-source-id* *step*))

(defcommand wpctl-source-volume-down () ()
  "Decrease the volume by N points"
  (volume-down *default-source-id* *step*))

(defcommand wpctl-source-mute () ()
  "Source mute"
  (mute *default-source-id*))

(defcommand wpctl-source-unmute () ()
  "Source unmute"
  (unmute *default-source-id*))

(defcommand wpctl-source-toggle-mute () ()
  "Toggle source Mute"
  (toggle-mute *default-source-id*))

(defcommand wpctl-source-set-volume (value) ((:string "Volume percentage:"))
  "Set source volume"
  (set-volume *default-source-id* value))
