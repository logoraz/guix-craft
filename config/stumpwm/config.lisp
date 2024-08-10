;;;; StumpWM Initialization File (config.lisp --> config)

;;; Commentary:
;;;

;;; References:
;;;


(in-package :stumpwm)

(setf *default-package* :stumpwm)

;;; Set PATHs: data directory, etc.
;; https://stumpwm.github.io/git/stumpwm-git_67.html
;; stumpwm still saves stuff to ~/.stumpwm.d, want it to go to
;; ~/.config/stumpwm/data/ instead.
(setf *data-dir* (concat (getenv "HOME")
                         "/.config/stumpwm/data/"))

;; Add StumpWM module paths (for GUIX system)
(defconstant +guix-share-path+ "/run/current-system/profile/share/")
(set-module-dir (concat +guix-share-path+
                        "common-lisp/sbcl/"))

;; A startup message can be used when initializing StumpWM, for now set to nil.
(setf *startup-message* nil)

;; TODO: Determine what the value of AltGr key is and then let's set it.
;; (setf *altgr-offset* 4)
;; (register-altgr-as-modifier)

;;; Initialize X11 Desktop Environment & Resources.
(load "~/.config/stumpwm/modules/auto-start.lisp")

;;; Stumpwm-contrib Packages/Libraries
;; Pipewire/Wirepluber Audio Controls for StumpWM
;; TODO: Refactor -> move to modeline file.
(add-to-load-path #p"~/.local/share/common-lisp/stumpwm-contrib/wpctl/")
(load-module "wpctl")
(setf wpctl:*modeline-fmt* "α %v")
(setf wpctl:*wpctl-path* "/home/logoraz/.guix-home/profile/bin/wpctl")
(setf wpctl:*mixer-command* "playerctl")

;; Simple Bluetooth Controls for StumpWM
;; TODO: Add modeline display
(add-to-load-path #p"~/.local/share/common-lisp/stumpwm-contrib/bluetooth/")
(load-module "bluetooth")

;;; Stumpwm-contrib packages not available in Guix
;; `end-session' - Provides session control commands, i.e. shutdown, restart,
;; and logoff for StumpWM.
(add-to-load-path #p"~/.local/share/common-lisp/stumpwm-contrib/end-session/")
(load-module "end-session")
;; Use loginctl instead of the default systemctl
(setf end-session:*end-session-command* "loginctl")

;;; Load in custom files
(load "~/.config/stumpwm/modules/syntax.lisp")
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
