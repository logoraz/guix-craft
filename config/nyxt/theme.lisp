;;;; Nyxt Theme Module

;;; Commentary:
;;; TODO: Create "nord-theme similar to the borrowed invader-theme, use same logic contruct
;;;       below.

;;; References:
;;; 1. Invader Theme:
;;;    - git clone https://github.com/atlas-engineer/invader
;;;      or
;;;    - wget nyxt-browser.com/static/release/invader.tar.gz
;;; 2. Sourced Saved in ~/.local/share/nyxt/extensions/invader

(in-package #:nyxt-user)


;;; Nord Color Palette
(defvar logoraz-nord0 "#2e3440")   ; 'Black'
(defvar logoraz-nord1 "#3b4252")   ; "Dark Gray'
(defvar logoraz-nord2 "#434c5e")   ; 'Medium Gray'
(defvar logoraz-nord3 "#4c566a")   ; 'Gray'
(defvar logoraz-nord4 "#d8dee9")   ; 'Light grey'
(defvar logoraz-nord5 "#e5e9f0")   ; 'Off-white'
(defvar logoraz-nord6 "#eceff4")   ; 'White'
(defvar logoraz-nord7 "#8fbcbb")   ; 'Blue/Green'
(defvar logoraz-nord8 "#88c0d0")   ; 'Teal'
(defvar logoraz-nord9 "#81a1c1")   ; 'Blue/Gray'
(defvar logoraz-nord10 "#5e81ac")  ; 'Blue'
(defvar logoraz-nord11 "#bf616a")  ; 'Red'
(defvar logoraz-nord12 "#d08770")  ; 'Orange'
(defvar logoraz-nord13 "#ebcb8b")  ; 'Yellow'
(defvar logoraz-nord14 "#a3be8c")  ; 'Green'
(defvar logoraz-nord15 "#b48ead")  ; 'Purple'


;;; Invader Theme
(defvar invader-theme
  (make-instance 'theme:theme
                 :dark-p t
                 :background-color- "#303240"
                 :background-color "#282A36"
                 :background-color+ "#1E2029"
                 :on-background-color "#F7FBFC"

                 :primary-color- "#679BCF"
                 :primary-color "#789FE8"
                 :primary-color+ "#7FABD7"
                 :on-primary-color "#0C0C0D"

                 :secondary-color- "#44475A"
                 :secondary-color "#44475A"
                 :secondary-color+ "#535A6E"
                 :on-secondary-color "#F7FBFC"

                 :action-color- "#6BE194"
                 :action-color "#4FDB71"
                 :action-color+ "#27BF4C"
                 :on-action-color "#0C0C0D"

                 :success-color- "#86D58E"
                 :success-color "#8AEA92"
                 :success-color+ "#71FE7D"
                 :on-success-color "#0C0C0D"

                 :highlight-color- "#EA43DD"
                 :highlight-color "#F45DE8"
                 :highlight-color+ "#FC83F2"
                 :on-highlight-color "#0C0C0D"

                 :warning-color- "#FCA904"
                 :warning-color "#FCBA04"
                 :warning-color+ "#FFD152"
                 :on-warning-color "#0C0C0D"

                 :codeblock-color- "#3C5FAA"
                 :codeblock-color "#355496"
                 :codeblock-color+ "#2D4880"
                 :on-codeblock-color "#F7FBFC"))

(define-configuration browser
    ((theme invader-theme)))

(define-configuration status-buffer
    ((style (str:concat %slot-value%
                        (theme:themed-css (theme *browser*))))))
