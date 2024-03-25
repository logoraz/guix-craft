;; Stump WM Frames Module
;; Setup:
;; ln -f ~/repos/guix-craft/config/stumpwm/modules/frames.lisp \
;;       ~/.config/stumpwm/frames.lisp


;; Set up Groups
(when *initializing*
  (grename "[EMACS]")
  (gnewbg  "[TERM]")
  (gnewbg  "[WWW]")
  (gnewbg  "[FILES]")
  (gnewbg  "[PRIV]"))

(clear-window-placement-rules)

;; Tell stumpwm to not honor application size hints
;; This was the cause of swm-gaps crashing with Emacs!
(setf *ignore-wm-inc-hints* t)

;; Show messages in the center
(setq *input-window-gravity*     :top
      *message-window-padding*   10
      *message-window-y-padding* 10
      *message-window-gravity    :top)

;; Gaps
(load-module "swm-gaps")

(setf swm-gaps:*head-gaps-size*  0
      swm-gaps:*inner-gaps-size* 5
     swm-gaps:*outer-gaps-size* 10)

(when *initializing*
 (swm-gaps:toggle-gaps))
