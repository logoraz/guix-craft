;; Stump WM Theme Module
;; Setup:
;; ln -f ~/repos/guix-craft/config/stumpwm/modules/theme.lisp \
;;       ~/.config/stumpwm/theme.lisp


;; Fonts
;; Enable TTF fonts
(load-module "ttf-fonts")
(setf xft:*font-dirs* (list (concat +guix-share-path+ "fonts/"))
      clx-truetype:+font-cache-filename+ (concat (getenv "HOME")
                                                 "/.local/share/fonts/"
                                                 "font-cache.sexp"))
(xft:cache-fonts)
(set-font (make-instance
           'xft:font
           :family "Fira Code"
           :subfamily "Regular" :size 11))

;; Colors
