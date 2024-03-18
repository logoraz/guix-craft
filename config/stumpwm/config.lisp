;;;; -*- mode: stumpwm-mode -*-
;;;; Stump WM Configuration File
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
;; (run-shell-command "autostart")
;; todo - put in autostart script...
(run-shell-command "feh --bg-scale ~/desktop/wallpapers/sunset-mountain.jpg ")
;; Run xmodmap to remap keys
;; (run-shell-command "xmodmap ~/.xmodmap")
(run-shell-command "xrdb -load ~/.Xresources")
;; (run-shell-command "xsetroot -cursor_name left_ptr")
;; (run-shell-command "xset b off")
;; (run-shell-command "xset s off")
;; (run-shell-command "export GDK_CORE_DEVICE_EVENTS=1")
;; (run-shell-command "xss-lock -- slock &")
;; (run-shell-command "picom &")


;; --- Load in custom file modules ---

;; (load "~/.config/stumpwm/modules/bluetooth.lisp")
(load "~/.config/stumpwm/modules/audio-wpctl.lisp")
;; (load "~/.config/stumpwm/modules/commands.lisp")
;; (load "~/.config/stumpwm/modules/placement.lisp")
;; (load "~/.config/stumpwm/modules/keybindings.lisp")
;; (load "~/.config/stumpwm/modules/theme.lisp")
;; (load "~/.config/stumpwm/modules/utilities.lisp")
(load "~/.config/stumpwm/modules/modeline.lisp")
;; (load "~/.config/stumpwm/modules/systemd.lisp")

;; Start the mode line
(when *initializing*
  (mode-line))

;; Mouse click should focus the window
(setf *mouse-focus-policy* :click
      *float-window-modifier* :SUPER)


;; Notify that everything is ready!
;; (setf *startup-message* "StumpWM is ready!")

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
                                                 "/.local/share/fonts/"
                                                 "font-cache.sexp"))
(xft:cache-fonts)
(set-font (make-instance
           'xft:font
           :family "Fira Code"
           :subfamily "Regular" :size 11))


;;;  --- Key Bindings ---

;; Enable multiple keyboard layouts (English and TBD)
(load-module "kbd-layouts")
(kbd-layouts:keyboard-layout-list "us")


;; Audio Controls
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
(define-key *top-map* (kbd "s-x") "exec xterm")
