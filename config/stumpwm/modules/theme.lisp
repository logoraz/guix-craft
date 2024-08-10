;;;; theme.lisp --> Theme settings for StumpWM

;;; Commentary:

;;; References:

(in-package :stumpwm)

;;; Fonts
;; Enable TTF fonts
(load-module "ttf-fonts")
(setf xft:*font-dirs* (list (concat +guix-share-path+ "fonts/"))
      clx-truetype:+font-cache-filename+ (concat (getenv "HOME")
                                                 "/.local/share/fonts/"
                                                 "font-cache.sexp"))
(xft:cache-fonts)
(set-font `(,(make-instance
              'xft:font :family "Hack"
                        :subfamily "Regular" :size 11 :antialias t)
            ,(make-instance
              'xft:font :family "JetBrains Mono"
                        :subfamily "Regular" :size 11 :antialias t)))
