;;;; -*- mode: stumpwm-mode -*-
;;;; Stump WM Initialization File
;;; Setup:
;;; ln -f ~/repos/guix-craft/config/stumpwm/config.lisp ~/.config/stumpwm/config


;;;  --- Initialization ---

(in-package :stumpwm)
(setf *default-package* :stumpwm)

;; --- Add StumpWM module paths (for GUIX) ---
(defconstant +guix-share-path+ (concat (getenv "HOME")
                                       "/.guix-home/profile/share/"))
(set-module-dir (concat +guix-share-path+
                        "common-lisp/sbcl/"))

;; A startup message can be used when initializing StumpWM, for now set to nil.
;; (setf *startup-message* nil)

;; TODO: Determine what the value of AltGr key is and then let's set it.
;; (setf *altgr-offset* 4)
;; (register-altgr-as-modifier)

;; --- Initialize Xorg resources ---
;; Needed for StumpWM config
;; (run-shell-command "autostart")
;;TODO: Put in autostart script?
;; Run xmodmap to remap keys
;; (run-shell-command "xmodmap ~/.xmodmap")
;; xrandr stuff would go here too?

;; --- Load in custom file modules ---

;; (load "~/.config/stumpwm/modules/macros.lisp")
(load "~/.config/stumpwm/modules/audio-wpctl.lisp")
;; (load "~/.config/stumpwm/modules/bluetooth.lisp")
(load "~/.config/stumpwm/modules/frames.lisp")
(load "~/.config/stumpwm/modules/keybindings.lisp")
(load "~/.config/stumpwm/modules/theme.lisp")
(load "~/.config/stumpwm/modules/modeline.lisp")
;; (load "~/.config/stumpwm/modules/utilities.lisp")

;; Start the mode line
;;TODO: Put in modeline module?
(when *initializing*
  (mode-line))

;; Mouse click should focus the window
;; Set super key to move floating windows
;;TODO maybe put in frames module...
(setf *mouse-focus-policy* :click
      *float-window-modifier* :SUPER)

;; Notify that everything is ready!
;; (setf *startup-message* "StumpWM is ready!")

;; --- Additional Xorg resources ---
;; XTerm configuration settings
(run-shell-command "xrdb -load ~/.Xresources")
