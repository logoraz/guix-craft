;;;; StumpWM Modeline Module

;;; Commentary:

;;; References:
;;; 1. StumpWM Contrib: https://github.com/stumpwm/stumpwm-contrib
;;; 2. battery-portible contrib
;;;    https://github.com/stumpwm/stumpwm-contrib/tree/master/modeline/battery-portable
;;;    in your .stumpwmrc. Battery information is then available via %B in your
;;;    mode-line config.
;;; 3. mem module contrib
;;;    https://github.com/stumpwm/stumpwm-contrib/tree/master/modeline/mem
;;;    Then you can use “%M” and/or “%N in your mode line format.
;;; 4. cpu module contrib
;;; From: https://github.com/stumpwm/stumpwm-contrib/tree/master/modeline/cpu
;;; Then you can use %C in your mode line format:

(in-package :stumpwm)

;; The timeout of the modeline indicates how often it refreshes in seconds.
(setf *mode-line-timeout* 2)

;;; Formatting options
(setf *time-modeline-string* "%F %H:%M")

;; Let’s also indicate how the groupname is displayed.
(setf *group-format* "%t")

;; The window format should display first its window number, then title -
;; limited to 30 characters.
(setf *window-format* " %n: %30t ")

;;; Load in colors module & Set Modeline Colors
(load "~/.config/stumpwm/modules/colors.lisp")

(setf *mode-line-background-color* logoraz-nord0
      *mode-line-foreground-color* logoraz-nord4)

(setf *mode-line-border-color* logoraz-nord3
      *mode-line-border-width* 1)

;;; Modeline Modules & Formatting
(load-module "cpu")
(load-module "mem")
(load-module "battery-portable")
(load-module "wifi")
;; (load-module "audio-wpclt")


(setf *screen-mode-line-format*
      (list "[%n]"             ; The current group's name
            "%v"               ; Windows
            "^>"               ; Push right
            " | %C"            ; CPU module
            " | %M"            ; Mem module
            " | %B"            ; Battery module
            " | %I"            ; Wifi
            " | %P"            ; wpctl volume
            " | %d"))          ; Clock


;; Customize what’s displayed in CPU module
(setf cpu::*cpu-modeline-fmt* "%c %t") ; default is "%c (%f) %t"

;; Set executable source for wifi module:
(setf wifi::*iwconfig-path* "/run/current-system/profile/sbin/iwconfig")
