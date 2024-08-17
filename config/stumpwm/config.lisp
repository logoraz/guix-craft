;;;; StumpWM Initialization File (config.lisp --> config)

;;; Commentary:
;;;

;;; References:
;;;


(in-package :stumpwm)

;; Define Guix profiles
(defconstant +guix-system-path+ "/run/current-system/profile/share/"
  "Define Guix System profile PATH.")
(defconstant +guix-home-path+ "/home/logoraz/.guix-home/profile/share/"
  "Define Guix Home profile PATH.")

;; Set StumpWM modules directory - at system level!
(set-module-dir (concat +guix-system-path+
                        "common-lisp/sbcl/"))

(setf *default-package* :stumpwm)

;;; Set PATHs: data directory, etc.
;; https://stumpwm.github.io/git/stumpwm-git_67.html
;; stumpwm still saves stuff to ~/.stumpwm.d, want it to go to
;; ~/.config/stumpwm/data/ instead.
(setf *data-dir* (concat (getenv "HOME")
                         "/.config/stumpwm/data/"))

;; A startup message can be used when initializing StumpWM, for now set to nil.
(setf *startup-message* nil)

;; TODO: Determine what the value of AltGr key is and then let's set it.
;; (setf *altgr-offset* 4)
;; (register-altgr-as-modifier)

;;; Initialize X11 Desktop Environment & Resources.
(load "~/.config/stumpwm/modules/auto-start.lisp")

;;; Stumpwm-contrib Packages/Libraries

;;; Load in custom files
(load "~/.config/stumpwm/modules/syntax.lisp")
(load "~/.config/stumpwm/modules/utilities.lisp")
(load "~/.config/stumpwm/modules/colors.lisp")
(load "~/.config/stumpwm/modules/frames.lisp")
(load "~/.config/stumpwm/modules/keybindings.lisp")
(load "~/.config/stumpwm/modules/theme.lisp")
(load "~/.config/stumpwm/modules/modeline.lisp")

;; Start the mode line
;; TODO: Put in modeline module?
(when *initializing*
  (mode-line))

;;; TODO maybe put in frames module...
(setf *mouse-focus-policy* :click ; Mouse click should focus the window
      *float-window-modifier* :SUPER) ; Set super key to move floating windows

;;; Navigate between windows from all workspaces
(load-module "globalwindows")

;;; Additional Xorg resources
(run-shell-command "xrdb -merge ~/.Xresources")

;;; Start StumpWM slynk server - persistent for session
;; Always hacking StumpWM
(require :slynk)
(sb-thread:make-thread
 (lambda () (slynk:create-server :port 4005 :dont-close t)))

;; Notify that everything is ready!
(setf *startup-message*
      (concatenate 'string "^2*Welcome ^Blogoraz^b! "
                  "Your ^BStumpWM^b session is ready."))
