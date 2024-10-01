;;;; frames.lisp --> Frames & Windows Configuration for StumpWM

;;; Author:
;;; Erik P Almaraz

;;; License:
;;; GPLv3

;;; Commentary:
;;;

;;; References:
;;;

(in-package :stumpwm)

;;; Window Groups & Placement
(when *initializing*
  (grename "[HOME]")
  (gnewbg  "[DEV]")
  (gnewbg  "[WWW]")
  (gnewbg  "[ETC]")
  (gnewbg  "[PRIV]"))

(clear-window-placement-rules)

(define-frame-preference "[HOME]" (nil t t :class "Tiling"))
(define-frame-preference "[DEV]"  (nil t t :class "Tiling"))
(define-frame-preference "[WWW]"  (nil t t :class "Tiling"))
(define-frame-preference "[ETC]"  (nil t t :class "Tiling"))
(define-frame-preference "[PRIV]" (nil t t :class "Tiling"))

(setf *dynamic-group-master-split-ratio* 1/2)

;;; X-window settings & stylization via gaps
;; Tell stumpwm to not honor application window size hints
;; This was the cause of swm-gaps crashing with Emacs!
(setf *ignore-wm-inc-hints* t)

;; Gaps
(load-module "swm-gaps")

(setf swm-gaps:*head-gaps-size*  0
      swm-gaps:*inner-gaps-size* 5
      swm-gaps:*outer-gaps-size* 10)

(when *initializing*
  (swm-gaps:toggle-gaps))

;;; Messaging & Input Windows
;; Show messages in the center
(set-border-color        logoraz-nord3)
(set-focus-color         logoraz-nord1)
(set-unfocus-color       logoraz-nord0)
(set-float-focus-color   logoraz-nord1)
(set-float-unfocus-color logoraz-nord0)

(set-fg-color logoraz-nord4)
(set-bg-color logoraz-nord0)

(setf *key-seq-color* "^6")
(setf *which-key-format* (concat *key-seq-color* "*~5a^n ~a"))

(setf *input-window-gravity*     :top-right
      *message-window-padding*   10
      *message-window-y-padding* 10
      *message-window-gravity    :top-right)

(setf *normal-border-width*       1
      *float-window-border*       1
      *float-window-title-height* 15
      *window-border-style*       :none
      *window-format*             "%n:%t")
