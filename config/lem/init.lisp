;;;; init.lisp - Lem Initialization File

;;; Erik P. Almaraz (logoraz)


;;; Load Additional CL Libraries
;; (pushnew "/path/to/lisp/project/" asdf:*central-registry* :test #'equal)



;;; Start Up & Basic Config

(in-package :lem-user)

;; Evaluating Lisp in M-: we want to be in Lem' package.
(lem-lisp-mode/internal::lisp-set-package "LEM")

;; Load Theme
;; (load-theme "decaf") ; default

;; Set Transparency
#+lem-sdl2
(sdl2-ffi.functions:sdl-set-window-opacity (lem-sdl2/display::display-window lem-sdl2/display::*display*) 0.84)

;; Logs on the terminal output:
;; (log:config :info)


;;; Setup Paredit
;; Borrowed from @gavinok (https://github.com/Gavinok/.lem)
(lem:add-hook lem:*find-file-hook*
              (lambda (buffer)
                (when (eq (buffer-major-mode buffer) 'lem-lisp-mode:lisp-mode)
                  (change-buffer-mode buffer 'lem-paredit-mode:paredit-mode t))))

;; Paredit Mappings
(define-key lem-paredit-mode:*paredit-mode-keymap* "Shift-Right"
  'lem-paredit-mode:paredit-slurp)
(define-key lem-paredit-mode:*paredit-mode-keymap* "Shift-Right"
  'lem-paredit-mode:paredit-barf)

;; FIXME - Seems to be causing an error
(lem:define-command paredit-quote-wrap () ()
  (progn
    (lem-paredit-mode:paredit-insert-doublequote)
    (lem-paredit-mode:paredit-slurp)
    (lem:delete-next-char)))

(define-key lem-paredit-mode:*paredit-mode-keymap* "M-\"" 'paredit-quote-wrap)



;;; Basic Mappings

;; Make undo & redo what I am used to
(defun custom-keybindings ()
  "Defining in a function to re-deploy after starting lem/legit after init."
  (define-key *global-keymap* "C-/" 'undo)
  (define-key *global-keymap* "C-_" 'redo)

  (define-key *global-keymap* "C-h B" 'describe-bindings)
  (define-key *global-keymap* "C-h k" 'describe-key)
  (define-key *global-keymap* "C-h a" 'apropos-command)
  (define-key *global-keymap* "C-h p" 'lem-lisp-mode:lisp-apropos-package))
(custom-keybindings) ; Enable custom keybindings on initialization.


;;; Commands

(define-command open-init-file () ()
  ;; @sasanidas
  (lem:find-file
   (merge-pathnames "init.lisp" (lem-home))))



;;; Completions
;; ref - @vindarel (https://github.com/vindarel/lem-init)

;; Choose the position of the completion prompt (new in May, 2024)
(setf lem-core::*default-prompt-gravity* :bottom-display)
(setf lem/prompt-window::*prompt-completion-window-gravity* :horizontally-above-window)
(setf lem/prompt-window::*fill-width* t)

;; and show the completion list directly, without a first press on TAB:
(add-hook *prompt-after-activate-hook*
          (lambda ()
            (call-command 'lem/prompt-window::prompt-completion nil)))

(add-hook *prompt-deactivate-hook*
          (lambda ()
            (lem/completion-mode:completion-end)))



;;; Experimental Packages

;; Using quicklisp -> issue with Guix finding micros...
;; See:  https://www.quicklisp.org/beta/

;; See Advanced Dependencies Management
;; -> https://lispcookbook.github.io/cl-cookbook/getting-started.html
;;    ASDF and Quicklisp loadable directories:
;;   |--> ~/common-lisp/
;;   |--> ~/quicklisp/local-projects/
;;   |--> ~/.local/share/common-lisp/source/

;; Quicklisp Initialization
;; Load quicklisp - not sure why it isn't being loaded in ~/.sbclrc file...
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp"
                                       (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

;; (bt:make-thread
;;  (lambda ()
;;    (ql:quickload :micros)))

;; According to garlic we don't need this...
;; (bt:make-thread
;;  (lambda ()
;;    (ql:quickload :micros)
;;    (micros:create-server :port 50000 :dont-close t)))

;; ;; Version Control
(define-command start-legit () ()
  "Lem Command to start legit in thread."
  (bt:make-thread
   (lambda ()
     (ql:quickload :lem/legit)
     ;; for some reason loading legit unsets keybindings and theme...
     (load-theme "decaf")
     (custom-keybindings))))




;;; Utilities

;; Now you can do `M-x time-stamp` to print the timestamp of the day, in the org-mode format:
;; "<2023-07-05 Wed>"
(load "~/.config/lem/lisp/time-stamp.lisp")

;; not currently working as I thought it was intended - deletes up to project directory
(load "~/.config/lem/lisp/file-prompt.lisp")

(load "~/.config/lem/lisp/utilities.lisp")





