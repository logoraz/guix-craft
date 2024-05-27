;; raz-base.el --- Base Config/Defaults -*- lexical-binding: t -*-

;; Author: Erik P. Almaraz

;; Commentary/References:
;; Default "Essential" Settings & Packages I use daily...
;; See `comp.el' for review of Andrea Corallo's legendary world on native
;; compilation (aka `eln' files).
;; Research difference between emacs-next-tree-sitter & emacs-next-pgtk
;; See https://www.emacswiki.org/emacs/PageBreaks
;;  ‘forward-page’ (`C-x ]’ or `C-]’),
;;  ‘backward-page’ (`C-x [’ or `C-[’), and `narrow-to-page' (‘C-x n p’).


;; Code:
;; TODO: Revmove dependencies on del-emacs-defhook macro...
;;       not a very efficient implementation, need to fix before it can be deployed...
;;TODO - define variables using `use-package' `:custom' keyword where possible.

;;
;; File Settings: Auto Save, Backups, History, Bookmark, and Recent Files.
;;

;; Auto Mode Alist
;; create `custom' file extension for Xdefaults/Xresources file types...
;; https://www.gnu.org/software/emacs/manual/html_node/efaq/Associating-modes-with-files.html
;; (add-to-list 'auto-mode-alist '("\\.xconf\\'" . conf-xdefaults-mode))

;; Auto Save: Prefix for generating auto-save-list-file-name
;; see - `auto-save-list-file-name'
(setq auto-save-list-file-prefix (expand-file-name "auto-save/.saves-"
                                                   *raz-var-directory*))
;; Backups
(setq  backup-directory-alist
       `(("." . ,(expand-file-name "backup" *raz-var-directory*)))
       make-backup-files t
       vc-make-backup-files nil
       backup-by-copying t
       version-control t
       delete-old-versions t
       kept-old-versions 6
       kept-new-versions 9
       delete-by-moving-to-trash t)

;; History
(use-package savehist
  :diminish savehist-mode
  :custom
  (savehist-save-minibuffer-history t)
  (savehist-file (expand-file-name "savehist.el" *raz-var-directory*))
  :config
  (setq history-length 500
        history-delete-duplicates t)
  (savehist-mode 1))

;; Bookmarks
(use-package bookmark
  :custom
  (bookmark-default-file (expand-file-name "bookmarks" *raz-var-directory*)))

;; Recent Files
(use-package recentf
  ;; recentf settings/hack
  ;; TODO: Optimize use-package configuration for this!
  :diminish recentf-mode
  :init
  (defun raz/advice-no-msg (orig &rest args)
    "Docstring tbd..."
    ;; Dynamic Scoping to the rescue.
    (let ((inhibit-message t))
      (apply orig args)))
  (setq recentf-save-file (expand-file-name "recentf" *raz-var-directory*)
        recentf-max-menu-items 50)
  ;; (customize-set-variable 'recentf-exlcude)

  ;; Makes a call to `load' which calls `message' because it's third argument is nil,
  ;; telling `load' to call `message'
  (advice-add 'recentf-cleanup :around #'raz/advice-no-msg)
  (advice-add 'recentf-load-list :around #'raz/advice-no-msg)
  :config
  (recentf-mode))


;; Coding/Editing Defaults
;;

(set-default-coding-systems 'utf-8)
(setq-default global-auto-revert-non-file-buffers t)
(setq-default indent-tabs-mode nil) ; use spaces instead of tabs
(setq-default cursor-type 'bar
              fill-column 90
              large-file-warning-threshold 100000000
              find-file-visit-truename t)
(global-auto-revert-mode 1)
(delete-selection-mode)
(column-number-mode 1)

(use-package display-fill-column-indicator
  :diminish
  :hook ((prog-mode . display-fill-column-indicator-mode)
         (lisp-interaction-mode . (lambda () (display-fill-column-indicator-mode -1))))
  :custom
  (display-fill-column-indicator-column fill-column))

(defun raz/switch-to-minibuffer ()
  "Switch to minibuffer window."
  (interactive)
  (if (active-minibuffer-window)
      (select-window (active-minibuffer-window))
    (error "Minibuffer is not active")))

(bind-key "C-c o" 'raz/switch-to-minibuffer)

(use-package eldoc
  :defer t
  :diminish eldoc-mode)

;;Dired setup
(use-package dired-x
  ;; Set dired-x buffer-local variables here.  For example:
  ;; (dired-omit-mode 1)
  :disabled
  :after dired)

(use-package ediff
  :defer t
  :custom
  (ediff-split-window-function 'split-window-horizontally)
  (ediff-window-setup-function 'ediff-setup-windows-plain)
  :config
  ;; Save & Restore Window configuration
  ;; https://www.emacswiki.org/emacs/EdiffMode
  (add-hook
   'ediff-load-hook
   (lambda ()
     (add-hook 'ediff-before-setup-hook
               (lambda ()
                 (setq ediff-saved-window-configuration
                       (current-window-configuration))))
     (let ((restore-window-configuration
            (lambda ()
              (set-window-configuration ediff-saved-window-configuration))))
       (add-hook 'ediff-quit-hook
                 restore-window-configuration
                 'append)
       (add-hook 'ediff-suspend-hook
                 restore-window-configuration
                 'append)))))


;;
;; External Package Path's Configuration
;;

;; Configure package PATH's
(use-package no-littering
  :demand t)

(use-package ligature
  ;; Fonts & Theme Configuration
  ;; Fira Code & Ligature Support
  ;; See: https://github.com/tonsky/FiraCode/wiki/Emacs-instructions#using-ligature
  ;; See: https://github.com/mickeynp/ligature.el
  :diminish ligature-mode
  :demand t
  :config
  (dolist
      (face
       '((default :font "Fira Code" :height 110)
         (fixed-pitch :font "Fira Code" :height 110)
         (variable-pitch :font "Iosevka Aile" :height 110)))
    (raz/set-face-attribute (car face) (cdr face)))
  ;; Enable the "www" ligature in every possible major mode
  (ligature-set-ligatures 't '("www"))
  ;; Enable traditional ligature support in eww-mode, if the
  ;; `variable-pitch' face supports it
  (ligature-set-ligatures 'eww-mode '("ff" "fi" "ffi"))
  ;; Enable all Cascadia Code ligatures in programming modes
  (ligature-set-ligatures
   'prog-mode
   '("|||>" "<|||" "<==>" "<!--" "####" "~~>" "***" "||=" "||>"
     ":::" "::=" "=:=" "===" "==>" "=!=" "=>>" "=<<" "=/=" "!=="
     "!!." ">=>" ">>=" ">>>" ">>-" ">->" "->>" "-->" "---" "-<<"
     "<~~" "<~>" "<*>" "<||" "<|>" "<$>" "<==" "<=>" "<=<" "<->"
     "<--" "<-<" "<<=" "<<-" "<<<" "<+>" "</>" "###" "#_(" "..<"
     "..." "+++" "/==" "///" "_|_" "www" "&&" "^=" "~~" "~@" "~="
     "~>" "~-" "**" "*>" "*/" "||" "|}" "|]" "|=" "|>" "|-" "{|"
     "[|" "]#" "::" ":=" ":>" ":<" "$>" "==" "=>" "!=" "!!" ">:"
     ">=" ">>" ">-" "-~" "-|" "->" "--" "-<" "<~" "<*" "<|" "<:"
     "<$" "<=" "<>" "<-" "<<" "<+" "</" "#{" "#[" "#:" "#=" "#!"
     "##" "#(" "#?" "#_" "%%" ".=" ".-" ".." ".?" "+>" "++" "?:"
     "?=" "?." "??" ";;" "/*" "/=" "/>" "//" "__" "~~" "(*" "*)"
     "\\\\" "://" ";;;" ";;;;" "!!!" "!!!!"))
  ;; Enables ligature checks globally in all buffers. You can also do it
  ;; per mode with `ligature-mode'.
  (global-ligature-mode t))

;;; Theme Configuration
;; Load in local copy of nord theme - to develop and customize...
(add-to-list 'custom-theme-load-path (expand-file-name "~/.config/emacs/themes/"))
(load-theme 'nord t)

(with-eval-after-load 'nord-theme
  ;; Set Custom defined "Nord" faces for modeline
  ;; See: list-faces-display for a list of defined faces.
  (dolist
      (face
       '((mode-line :box (:line-width 1 :color "#7b88a1" :style none))
         (mode-line :foreground "#D8DEE9")
         (mode-line-inactive :box (:line-width 1 :color "#616e88" :style none))
         (mode-line-inactive :foreground "#7b88a1")
         (fill-column-indicator :foreground "#3B4252")))
    (raz/set-face-attribute (car face) (cdr face))))

(use-package tab-bar
  :after nord-theme
  :config
    ;; Set custome definte "Nord" faces for tab-bar
  (dolist
      (face
       '((tab-bar :foreground "#7b88a1" :background "#272C37")
         (tab-line :inherit tab-bar)
         (tab-bar-tab :inherit mode-line-highlight
                      :foreground "#b48ead"
                      :background "#272C37")
         (tab-bar-tab :box (:line-width 1 :color "#7b88a1" :style none))
         (tab-bar-tab-group-current :inherit tab-bar-tab)
         (tab-bar-tab-group-current :box (:line-width 1 :color "#3B4252" :style none))
         (tab-bar-tab-inactive :foreground "#7b88a1" :background "#272C37")
         (tab-bar-tab-inactive :box (:line-width 1 :color "#616e88" :style none))
         (tab-bar-tab-group-inactive :inherit tab-bar-tab-inactive)
         (tab-bar-tab-ungrouped :inherit tab-bar-tab-inactive)))
    (raz/set-face-attribute (car face) (cdr face))))


;; Editing/IDE Package configurations
(use-package undo-tree
  :diminish undo-tree-mode
  :demand t
  :custom
  (undo-tree-history-directory-alist
   `(("." . ,(expand-file-name "undo-tree-hist/"
                               *raz-var-directory*))))
  :config
  (setq kill-do-not-save-duplicates t)
  (global-undo-tree-mode))

(use-package paredit
  :diminish paredit-mode
  :hook ((eval-expression-minibuffer-setup
          lisp-interaction-mode
          emacs-lisp-mode
          lisp-mode
          scheme-mode
          org-mode) . enable-paredit-mode))

(use-package ws-butler
  :diminish ws-butler-mode
  :hook ((text-mode prog-mode) . ws-butler-mode))

(use-package magit
  :defer 5
  :custom
  (magit-clone-always-transient nil)
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)
  (vc-follow-symlinks t))

;; Workflow frame/tab workspaces
(use-package beframe
  :disabled
  :diminish beframe-mode
  :bind-keymap ("C-c b" . beframe-prefix-map)
  :custom
  (beframe-global-buffers '("*scratch*" "*Messages*" "*Backtrace*"))
  :config
  (beframe-mode 1))

(use-package tabspaces
  ;; Not available in Guix so need to use melpa...
  :ensure t
  :diminish tabspaces-mode
  :custom
  (tabspaces-use-filtered-buffers-as-default t)
  (tabspaces-session t)
  ;; (tabspaces-session-auto-restore t)
  :config
  (tabspaces-mode 1))





(provide 'raz-base)
