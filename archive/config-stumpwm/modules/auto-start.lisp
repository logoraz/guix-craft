;;;; auto-start.lisp


(in-package :stumpwm)

;;; X11 Settings

;; Run xmodmap to remap keys
;; (run-shell-command "xmodmap ~/.xmodmap")
;; xrandr stuff would go here too?

;; Set pointer/cursor paths & defaults -> theme set in .Xresources
(run-shell-command "xsetroot -xcf ~/.icons/XCursor-Pro-Dark/cursors/left_ptr 22")

;; Turn off system bell & screen-saver control
(run-shell-command "xset b off")
(run-shell-command "xset s off")

;;; UI Settings

;; Set Wallpaper
(run-shell-command "feh --bg-scale  ~/desktop/wallpapers/sunset-mountain.jpg")

;; Enable screen compositing
(run-shell-command "picom")

;; Enable screen locking on suspend
(run-shell-command "xss-lock -- slock")
