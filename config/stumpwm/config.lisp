;;;; StumpWM Initialization File (config)
;;; Commentary:
;;; TODO: 1. Research into the differences between `require' & `use-module'
;;;       2. Implment in StumpWM config & modules the more performant method.

;;; References:


(in-package :stumpwm)

(setf *default-package* :stumpwm)

;;; Set PATHs: data directory, etc.
;; https://stumpwm.github.io/git/stumpwm-git_67.html
;; stumpwm still saves stuff to ~/.stumpwm.d, want it to go to ~/.config/stumpwm/data/ instead.
(setf *data-dir* (concat (getenv "HOME")
                         "/.config/stumpwm/data/"))

;; Add StumpWM module paths (for GUIX)
(defconstant +guix-share-path+ (concat (getenv "HOME")
                                       "/.guix-home/profile/share/"))
(set-module-dir (concat +guix-share-path+
                        "common-lisp/sbcl/"))

;; A startup message can be used when initializing StumpWM, for now set to nil.
(setf *startup-message* nil)

;; TODO: Determine what the value of AltGr key is and then let's set it.
;; (setf *altgr-offset* 4)
;; (register-altgr-as-modifier)

;;; Initialize Desktop Environment & Xorg Resources.
;; TODO: Create Package/Librarie to submit to StumpWM/contrib
(load "~/.config/stumpwm/modules/auto-start.lisp")

;;; Modified StumpWM contrib module
;; TODO - Determine general fix -> send PR & possibly package for Guix!
;; 1. Fork repo this was borrowed from and apply changes
;;    - https://github.com/Junker/stumpwm-wpctl
;; 2. Then setup as stand-alone package & trial out
(load "~/.config/stumpwm/modules/audio-wpctl.lisp")

;;; StumpWM contrib not available in Guix
;;; end-session
;; Need to add to load-path as it is not part of Guix available packages
(add-to-load-path #p"~/.local/share/common-lisp/stumpwm-contrib/util/end-session/")
;; actually load the module
(load-module "end-session")
;; Use loginctl instead of the default systemctl
;; (setf end-session:*end-session-command* "loginctl")

;;; Load in custom file modules
(load "~/.config/stumpwm/modules/commands.lisp")
(load "~/.config/stumpwm/modules/utilities.lisp")
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
;; XTerm configuration settings
;; (run-shell-command "xrdb -load ~/.Xresources")
(run-shell-command "xrdb -merge ~/.Xresources")

;; Start Slynk/Sly server to connect Emacs to StumpWM -> port 4005
(require :slynk)
(slynk:create-server :dont-close t)

;; Notify that everything is ready!
(setf *startup-message* "^2*Welcome ^Blogoraz^b! Your ^BStumpWM^b session is ready.")
