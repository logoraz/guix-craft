;;;; -*- mode: stumpwm-mode -*-
;;;; Stump WM Configuration File
;;; Setup:
;;; ln -f ~/repos/guix-craft/config/stumpwm/config.lisp ~/.config/stumpwm/config

;;;  --- Initialization ---

(in-package :stumpwm)
(setf *default-package* :stumpwm)

;; --- Add StumpWM module paths (for GUIX) ---
(defconstant +guix-share-path+ "/home/raiz/.guix-home/profile/share/")

(set-module-dir (concat +guix-share-path+
                        "common-lisp/sbcl/"))


;;;  --- Environment setup ---

;; Set up Groups
(when *initializing*
  (grename "[EMACS]")
  (gnewbg  "[TERM]")
  (gnewbg  "[WWW]")
  (gnewbg  "[FILES]")
  (gnewbg  "[PRIV]"))

(clear-window-placement-rules)

;; Tell stumpwm to not honor application size hints
;; This was the cause of swm-gaps crashing with Emacs!
(setf *ignore-wm-inc-hints* t)

;; Change the prefix key to Super-d
(set-prefix-key (kbd "s-d"))

;; Mouse click should focus the window
(setf *mouse-focus-policy* :click
      *float-window-modifier* :SUPER)

;; Show messages in the center
(setq *input-window-gravity*     :top
      *message-window-padding*   10
      *message-window-y-padding* 10
      *message-window-gravity    :top)


;;;  --- Visual Enhancements ---

;; Gaps
(load-module "swm-gaps")

(setf swm-gaps:*head-gaps-size*  0
      swm-gaps:*inner-gaps-size* 5
     swm-gaps:*outer-gaps-size* 10)

(when *initializing*
 (swm-gaps:toggle-gaps))

;; Enable TTF fonts
(load-module "ttf-fonts")
(setf xft:*font-dirs* (list (concat +guix-share-path+ "fonts/"))
      clx-truetype:+font-cache-filename+ (concat (getenv "HOME")
                                                 "/.fonts/font-cache.sexp"))
(xft:cache-fonts)
(set-font (make-instance
           'xft:font
           :family "Fira Code"
           :subfamily "Regular" :size 11))


;;;  --- Mode line ---

;; Set mode line colors
(setf *mode-line-background-color* "#272C37"
      *mode-line-foreground-color* "#d8dee9")

;; Start the mode line
(when *initializing*
  (mode-line))


;;;  --- Window Placement Rules ---

;;

;;;  --- Start initial applications ---
(run-shell-command "feh --bg-scale ~/desktop/wallpapers/sunset-mountain.jpg ")


;;;  --- Key Bindings ---

;; Enable multiple keyboard layouts (English and TBD)
(load-module "kbd-layouts")
(kbd-layouts:keyboard-layout-list "us")

;; Run xmodmap to remap keys
;; (run-shell-command "xmodmap ~/.dotfiles/.config/i3/Xmodmap")

;; Audio Controls
(defparameter *step* 5)
(defparameter *wpctl-path* "/home/raiz/.guix-home/profile/bin/wpctl")
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

(define-key *top-map* (kbd "XF86AudioRaiseVolume") "wpctl-volume-up")
(define-key *top-map* (kbd "XF86AudioLowerVolume") "wpctl-volume-down")
(define-key *top-map* (kbd "XF86AudioMute") "wpctl-toggle-mute")

;; Brightness Controls
;; brightnessct

(define-key *top-map* (kbd "XF86MonBrightnessDown")
            "exec brightnessctl set 5%-")
(define-key *top-map* (kbd "XF86MonBrightnessUp")
            "exec brightnessctl set +5%")

;; Groups

(define-key *top-map* (kbd "s-1") "gselect 1")
(define-key *top-map* (kbd "s-2") "gselect 2")
(define-key *top-map* (kbd "s-3") "gselect 3")
(define-key *top-map* (kbd "s-4") "gselect 4")
(define-key *top-map* (kbd "s-5") "gselect 5")


;; Applications

(define-key *top-map* (kbd "s-n") "exec nyxt")
