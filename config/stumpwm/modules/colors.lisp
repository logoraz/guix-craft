;;;; colors.lisp --> StumpWM Nord Colors Module

;;; Commentary:

;;; References:

(in-package :stumpwm)

;;; Nord Color Palette
(defvar logoraz-nord0 "#2e3440")  ; 'Black'
(defvar logoraz-nord1 "#3b4252")  ; "Dark Gray'
(defvar logoraz-nord2 "#434c5e")  ; 'Medium Gray'
(defvar logoraz-nord3 "#4c566a")  ; 'Gray'
(defvar logoraz-nord4 "#d8dee9")  ; 'Light grey'
(defvar logoraz-nord5 "#e5e9f0")  ; 'Off-white'
(defvar logoraz-nord6 "#eceff4")  ; 'White'
(defvar logoraz-nord7 "#8fbcbb")  ; 'Blue/Green'
(defvar logoraz-nord8 "#88c0d0")  ; 'Teal'
(defvar logoraz-nord9 "#81a1c1")  ; 'Blue/Gray'
(defvar logoraz-nord10 "#5e81ac") ; 'Blue'
(defvar logoraz-nord11 "#bf616a") ; 'Red'
(defvar logoraz-nord12 "#d08770") ; 'Orange'
(defvar logoraz-nord13 "#ebcb8b") ; 'Yellow'
(defvar logoraz-nord14 "#a3be8c") ; 'Green'
(defvar logoraz-nord15 "#b48ead") ; 'Purple'

(setq *colors* (list
                logoraz-nord1     ; 0 Black
                logoraz-nord11    ; 1 Red
                logoraz-nord14    ; 2 Green
                logoraz-nord13    ; 3 Yellow
                logoraz-nord10    ; 4 Blue
                logoraz-nord14    ; 5 Magenta -> 'Green'
                logoraz-nord8     ; 6 Cyan
                logoraz-nord5     ; 7 White
                ;; Extra Colors
                logoraz-nord12    ; 8 optional-1 - 'Orange'
                logoraz-nord15))  ; 9 optional-2 - 'Purple'

(when *initializing*
  (update-color-map (current-screen)))
