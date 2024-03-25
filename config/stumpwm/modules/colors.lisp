;; Stump WM Nord Colors Module
;; Setup:
;; ln -f ~/repos/guix-craft/config/stumpwm/modules/colors.lisp \
;;       ~/.config/stumpwm/colors.lisp


;; Nord Color Palette
(defvar logoraz-nord0 "#2e3440")
(defvar logoraz-nord1 "#3b4252")
(defvar logoraz-nord2 "#434c5e")
(defvar logoraz-nord3 "#4c566a")
(defvar logoraz-nord4 "#d8dee9")
(defvar logoraz-nord5 "#e5e9f0")
(defvar logoraz-nord6 "#eceff4")
(defvar logoraz-nord7 "#8fbcbb")
(defvar logoraz-nord8 "#88c0d0")
(defvar logoraz-nord9 "#81a1c1")
(defvar logoraz-nord10 "#5e81ac")
(defvar logoraz-nord11 "#bf616a")
(defvar logoraz-nord12 "#d08770")
(defvar logoraz-nord13 "#ebcb8b")
(defvar logoraz-nord14 "#a3be8c")
(defvar logoraz-nord15 "#b48ead")

(setq *colors*
      `(,logoraz-nord1   ;; 0 black
        ,logoraz-nord11  ;; 1 red
        ,logoraz-nord14  ;; 2 green
        ,logoraz-nord13  ;; 3 yellow
        ,logoraz-nord10  ;; 4 blue
        ,logoraz-nord14  ;; 5 magenta
        ,logoraz-nord8   ;; 6 cyan
        ,logoraz-nord5)) ;; 7 white

(when *initializing*
  (update-color-map (current-screen)))
