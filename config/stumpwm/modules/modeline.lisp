;;;; -*- mode: stumpwm-mode -*-
;;;; Stump WM Modeline Module
;;; Setup:
;;; ln -f ~/repos/guix-craft/config/stumpwm/modules/modeline.lisp \
;;;       ~/.config/stumpwm/theme.lisp


;; The timeout of the modeline indicates how often it refreshes in seconds.
(setf *mode-line-timeout* 2)

;; Formatting optionsn
(setf *time-modeline-string* "%F %H:%M")
;; Let’s also indicate how the groupname is displayed.
(setf *group-format* "%t")
;; The window format should display first its window number, then its titled,
;; limited to 30 characters.
(setf *window-format* " %n: %30t ")

;; Load in colors module
(load "~/.config/stumpwm/modules/colors.lisp")

(setf *mode-line-background-color* logoraz-nord1
      *mode-line-foreground-color* logoraz-nord5)

;; (setf *mode-line-background-color* "#272C37"
;;       *mode-line-foreground-color* "#d8dee9")

;; --- modeline modules ---
;; From: https://github.com/stumpwm/stumpwm-contrib
(load-module "cpu")
(load-module "mem")
(load-module "battery-portable")

;; https://github.com/stumpwm/stumpwm-contrib/tree/master/modeline/battery-portable
;; in your .stumpwmrc. Battery information is then available via %B in your
;; mode-line config.

;; https://github.com/stumpwm/stumpwm-contrib/tree/master/modeline/mem
;;Then you can use “%M” and/or “%N in your mode line format.

;; From: https://github.com/stumpwm/stumpwm-contrib/tree/master/modeline/cpu
;; Then you can use %C in your mode line format:
(setf *screen-mode-line-format*
      (list "[%n]"                      ; Groups
            "%v"                        ; Windows
            "^>"                        ; Push right
            " | %C"                     ; CPU module
            " | %M"                     ; Mem module
            " | %B"                     ; Battery module
            " | %d"))                   ; Clock

;; You can customize what’s displayed in CPU module by changing the
;; cpu::*cpu-modeline-fmt* internal variable in your init.lisp:
(setf cpu::*cpu-modeline-fmt* "%c %t") ; default is "%c (%f) %t"
