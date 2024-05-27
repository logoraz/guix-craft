;; raz-media.el --- Multimedia Management -*- lexical-binding: t; -*-

;; Author: Erik P. Almaraz

;; Commentary/References:
;; See MPC (bundled in Emacs): https://www.emacswiki.org/emacs/Mpc
;; See EMMS: https://www.emacswiki.org/emacs/EMMS


;; Code:

(use-package bongo
  :if (eq system-type 'gnu/linux)
  :bind ("C-c p" . bongo-dired-library-mode)
  :custom
  (bongo-default-directory "~/files/media/")
  (bongo-prefer-library-buffers nil)
  (bongo-insert-whole-directory-trees t)
  (bongo-header-line-mode nil)
  (bongo-mode-line-indicator-mode nil)
  (bongo-display-track-icons t)
  (bongo-display-track-lengths t)
  (bongo-display-header-icons t)
  (bongo-display-playback-mode-indicator t)
  (bongo-display-inline-playback-progress t)
  (bongo-mark-played-tracks t)
  (bongo-field-separator (propertize " · " 'face 'shadow))
  (bongo-enabled-backends '(vlc mpv))
  :config
  (setq bongo-logo nil))




(provide 'raz-media)
