;;;; -*- mode: stumpwm-mode -*-
;;;; Stump WM Keybindings Module
;;; Setup:
;;; ln -f ~/repos/guix-craft/config/stumpwm/modules/keybindings.lisp \
;;;       ~/.config/stumpwm/keybindings.lisp


;; Change the prefix key to Super-d
(set-prefix-key (kbd "s-d"))

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
;;TODO - remove as I get nyxt configuration fully functional..
(define-key *top-map* (kbd "s-i") "exec icecat")
