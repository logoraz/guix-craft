;; raz-lisp-ide.el --- Lisp IDE via SLY -*- lexical-binding: t -*-

;; Author: Erik P. Almaraz

;; Commentary:
;;

;; References
;; 1. http://joaotavora.github.io/sly/#A-SLY-tour-for-SLIME-users
;; 2. Source: https://github.com/joaotavora/sly
;;
;; TODO:
;;   1. Setup/Configure Nyxt extension
;;   2. Setup/Configure stumpwm-mode extension
;;   3. Look into:  M-. -> runs the command sly-edit-definition (found in sly-mode-map),
;;      which is an interactive native-compiled Lisp function in ‘sly.el’.


;; Code:


(use-package sly
  ;; Enable sly IDE for Common Lisp
  :hook (;; (sly-mode . raz/sly-auto-connect)
         (lisp . sly-editing-mode))
  :custom
  (inferior-lisp-program (executable-find "sbcl")
                         "Set default lisp to Steel Bank Common Lisp.")
  (sly-lisp-implementations
   '((ccl (executable-find "ccl"))
     (sbcl (executable-find "sbcl") :coding-system utf-8-unix)
     ;; Enable development of sly
     (nyxt-sbcl
           (lambda () (nyxt-make-guix-cl-for-nyxt
                  "~/common-lisp/nyxt"
                  :force t
                  :cl-implementation "sbcl"
                  :cl-system "nyxt/electron"
                  :no-grafts t
                  :ad-hoc '("emacs" "xdg-utils" "git")))))
   "Set Lisp Compilers: Invoke sly with a prefix, M-- M-x sly RET nyxt-sbcl RET")
  :config
  (load "~/common-lisp/nyxt/build-scripts/nyxt-guix.el" :noerror)

  (defun raz/sly-nyxt-dev-connect ()
    "Initiate Nyxt developtment environement for electron renderer."
    (interactive)
    ;;FIXME -> query if nyxt-slynk is running or has been started (unless ...)
    ;; (save-excursion (sly-connect "localhost" 4006))
    (asdf:load-system :nyxt/electron)
    (nyxt:start))

  (defun raz/nyxt-run-test-suit ()
    "Run Nyxt test suit for electron renderer."
    (interactive)
    (asdf:test-system :nyxt/electron))

  (defun raz/sly-nyxt-auto-connect ()
    "Auto connect to Nyxt slynk session, start via start-slynk Nyxt command -> port 4006."
    (interactive)
    ;;FIXME -> query if nyxt-slynk is running or has been started (unless ...)
    (save-excursion (sly-connect "localhost" 4006)))

  ;; See: https://joaotavora.github.io/sly/#Loading-Slynk-faster
  (defun raz/sly-auto-connect ()
    (unless (sly-connected-p)
      (save-excursion (sly)))))

;; Have these packages here as place-holders -> still not sure if I want to use them
;; yet...
(use-package nyxt
  :disabled
  :bind-keymap ("C-c y" . nyxt-map)
  :after sly                            ;?
  :config
  (setq nyxt-path (executable-find "nyxt"))

  (setq nyxt-startup-flags
        '("shell" "-D" "-f"
          "path/to/nyxt/build-scripts/nyxt.scm"
          "--"
          "path/to/nyxt/nyxt"
          "-e"
          "(start-slynk)")))

(use-package stumpwm-mode
  :disabled
  :after sly ;?
  :config
  (defun raz/stumpwm--send-command (command)
    (start-process-shell-command "stumpish" nil (concat  "stumpish " command)))

  (set-frame-parameter (selected-frame) 'name "Emacs")
  (select-frame (make-frame '((name . "Chat"))))
  (persp-switch "Chat")
  (persp-kill "Main")
  (select-frame-by-name "Emacs"))




(provide 'raz-lisp-ide)
