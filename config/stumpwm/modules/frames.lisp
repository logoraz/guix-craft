;;;; frames.lisp --> Frames & Windows Configuration for StumpWM

;;; Commentary:

;;; References:

(in-package :stumpwm)

;;; Set up Groups & Placement
(when *initializing*
  (grename "[HOME]")
  (gnewbg  "[TERM]")
  (gnewbg  "[WWW]")
  (gnewbg  "[PRIV]")
  (gnewbg  "[ETC]"))

(clear-window-placement-rules)

(define-frame-preference "EMACS" (nil t t :class "Tiling"))
(define-frame-preference "DEV" (nil t t :class "Tiling"))
(define-frame-preference "WWW" (nil t t :class "Tiling"))
(define-frame-preference "FILES" (nil t t :class "Tiling"))
(define-frame-preference "PRIV" (nil t t :class "Tiling"))

(setf *dynamic-group-master-split-ratio* 1/2)

;;; X-window settings & stylization via gaps
;; Tell stumpwm to not honor application window size hints
;; This was the cause of swm-gaps crashing with Emacs!
(setf *ignore-wm-inc-hints* t)

;; Show messages in the center
(setq *input-window-gravity*     :top
      *message-window-padding*   10
      *message-window-y-padding* 10
      *message-window-gravity    :top)

;;; Gaps
(load-module "swm-gaps")

(setf swm-gaps:*head-gaps-size*  0
      swm-gaps:*inner-gaps-size* 5
      swm-gaps:*outer-gaps-size* 10)

(when *initializing*
  (swm-gaps:toggle-gaps))

