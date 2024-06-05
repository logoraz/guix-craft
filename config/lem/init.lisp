;;;; init.lisp - Lem Initialization File

;;; Erik P. Almaraz (logoraz)

;;; Notes:


(in-package :lem-user)

;; (load-theme "decaf")



;;; Basic Config
;; Set Transparency (using SDL2 Lem Backend)
(sdl2-ffi.functions:sdl-set-window-opacity
 (lem-sdl2/display:display-window (lem-sdl2/display:current-display)) 0.84)

(define-command open-init-file () ()
  ;; @sasanidas
  (lem:find-file
   (merge-pathnames "init.lisp" (lem-home))))



;;; Borrowed from Gavinok
;;; Setup Paredit
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
;; (lem:define-command paredit-quote-wrap () ()
;;   (progn
;;     (lem-paredit-mode:paredit-insert-doublequote)
;;     (lem-paredit-mode:paredit-slurp)
;;     (lem:delete-next-char)))

;; (define-key lem-paredit-mode:*paredit-mode-keymap* "M-\"" 'paredit-quote-wrap)



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



;;; Experimental Packages

;; Slime/Swank REPL configuration
;; Using quicklisp -> issue with Guix finding micros...
;; See:  https://www.quicklisp.org/beta/
;; modified 'quicklisp/setup.lisp' -> 'common-lisp/quicklisp/setup.lisp'
;; Alternative solution
;; (setf lem-lisp-mode/implementation::*default-command* 
;;       "sbcl --eval \"(asdf:load-system :micros)\"")

;; Load quicklisp - not sure why it isn't being loaded in ~/.sbclrc file...
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp"
                                       (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

;; check open ports $ss -lntp
(bt:make-thread
 (lambda ()
   (ql:quickload :micros)))

(bt:make-thread
 (lambda () 
   (micros:create-server :port 50000 :dont-close t)))

;; ;; Version Control
(define-command start-legit () ()
  "Lem Command to start legit in thread."
  (bt:make-thread
   (lambda ()
     (ql:quickload :lem/legit)
     (load-theme "decaf")
     (custom-keybindings))))

