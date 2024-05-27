;;;; StumpWM Keybindings Module

;;; Commentary:

;;; References:

(in-package :stumpwm)

;;; Enable multiple keyboard layouts (English and TBD)

(load-module "kbd-layouts")
 (kbd-layouts:keyboard-layout-list "us")

;;;  Defaults s-SPC for this command, reset & set this to prefix-key below!
(define-key *top-map* (kbd "s-k") "switch-keyboard-layout")

;;; Change the prefix key to Super-SPACE
(set-prefix-key (kbd "s-SPC"))

;;; Enable which-key
(which-key-mode)

;;; Audio/Mic Controls
(define-key *top-map* (kbd "XF86AudioRaiseVolume") "wpctl-volume-up")
(define-key *top-map* (kbd "XF86AudioLowerVolume") "wpctl-volume-down")
(define-key *top-map* (kbd "XF86AudioMute") "wpctl-toggle-mute")
(define-key *top-map* (kbd "XF86AudioMicMute") "wpctl-source-toggle-mute")

;;; Brightness Controls
;; TODO - create a module for brightness controls that also also display on modeline.
(define-key *top-map* (kbd "XF86MonBrightnessDown")
  "exec brightnessctl set 5%-")
(define-key *top-map* (kbd "XF86MonBrightnessUp")
  "exec brightnessctl set +5%")

;;; Groups
(define-key *top-map* (kbd "s-1") "gselect 1")
(define-key *top-map* (kbd "s-2") "gselect 2")
(define-key *top-map* (kbd "s-3") "gselect 3")
(define-key *top-map* (kbd "s-4") "gselect 4")
(define-key *top-map* (kbd "s-5") "gselect 5")

;;; Applications
(defvar *my-applications-keymap*
  (let ((key-map (make-sparse-keymap)))
    (define-key key-map (kbd "x") "exec ~/.config/xorg/start-xterm.sh")
    (define-key key-map (kbd "n") "exec ~/.config/nyxt/start-nyxt.sh")
    (define-key key-map (kbd "N") "exec nyxt")
    (define-key key-map (kbd "l") "exec lem")
    (define-key key-map (kbd "I") "exec icecat")
    (define-key key-map (kbd "g") "exec gimp")
    (define-key key-map (kbd "i") "exec inkscape")
    (define-key key-map (kbd "b") "exec blender")
    (define-key key-map (kbd "c") "exec gnucash")
    (define-key key-map (kbd "k") "exec keepassxc")
    (define-key key-map (kbd "e") "exec emacsclient -c")
    (define-key key-map (kbd "s") '*my-screenshot-keymap*)
    key-map))
(define-key *root-map* (kbd "a") '*my-applications-keymap*)

;;; Screenshots
(defvar *my-screenshot-dir* "~/desktop/screenshots/"
  "Path to custom screenshots directory.")

(defvar *my-screenshot-keymap*
  (let ((key-map (make-sparse-keymap)))
    (define-key key-map (kbd "s") "screenshot")
    (define-key key-map (kbd "w") "screenshot-window")
    (define-key key-map (kbd "a") "screenshot-area")
    key-map))

(define-key *top-map* (kbd "Print") '*my-screenshot-keymap*)
