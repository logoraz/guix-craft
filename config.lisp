;;;; -*- mode: stumpwm-mode -*-
;;;; Stump WM Configuration File
;;; Setup:
;;; ln -s ~/.config/emacs/stumpwm/config.lisp ~/.config/stumpwm/config

;;;  --- Initialization ---

(in-package :stumpwm)
(setf *default-package* :stumpwm)

;; --- Add StumpWM module paths (for GUIX) ---
(defconstant +guix-share-path+ "/home/erik/.guix-home/profile/share")

(set-module-dir (concat +guix-share-path+
                        "/common-lisp/sbcl/"))

;;;  --- Environment setup ---

;; Set up Groups
(when *initializing*
  (grename "[EMACS]")
  (gnewbg  "[WWW]")
  (gnewbg  "[TERM]")
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

;;;  --- Key Bindings ---

;; Enable multiple keyboard layouts (English and TBD)
(load-module "kbd-layouts")
(kbd-layouts:keyboard-layout-list "us")
;; (setf kbd-layouts:*caps-lock-behavior* :ctrl)

;; Run xmodmap to remap keys
;; (run-shell-command "xmodmap ~/.dotfiles/.config/i3/Xmodmap")

;; Set some super key bindings

;;;  --- Visual Enhancements ---

;; Get gapped
(load-module "swm-gaps")

(setf swm-gaps:*head-gaps-size*  0
      swm-gaps:*inner-gaps-size* 5
      swm-gaps:*outer-gaps-size* 10)

(when *initializing*
  (swm-gaps:toggle-gaps))

;; Enable TTF fonts
(load-module "ttf-fonts")
(setf xft:*font-dirs* (list (concat +guix-share-path+ "/fonts/"))
      clx-truetype:+font-cache-filename+ (concat "/home/erik/.local/share"
                                                 "/fonts/font-cache.sexp"))

(xft:cache-fonts)
(set-font (make-instance
           'xft:font
           :family "Fira Code Retina"
           :subfamily "Regular" :size 10))

;;;  --- Mode line ---

;; Set mode line colors
(setf *mode-line-background-color* "#272C37"
      *mode-line-foreground-color* "#d8dee9")

;; Start the mode line
(when *initializing*
  (mode-line))

;; Add the system tray module
;; (load-module "stumptray")

;; (when *initializing*
;;   (stumptray:stumptray))

;;;  --- Window Placement Rules ---

;;

;;;  --- Start initial applications ---

;; (run-shell-command "polybar panel")
(run-shell-command "feh --bg-scale ~/Pictures/Wallpapers/sunset-mountain.jpg ")
(run-shell-command "dunst")
(run-shell-command "nm-applet")
(run-shell-command "pasystray")

