;;;; file-prompt.lisp
;;; Description:
;;; Adapted from https://github.com/garlic0x1/.lem/blob/master/src/file-prompt.lisp
;;; Use C-Backspace to delete up to next directory
;;; This modifies the default which stops at hyphens and/or special characters, whereas
;;; this goes up only to '/', i.e. next directory

(in-package :lem)

(define-command garlic/find-file () ()
  "find-file with backspace bound to up-directory."
  (let ((keys (make-keymap)))
    (define-key keys "Backspace" 'fermin/up-directory)
    (with-special-keymap (keys)
      (call-command 'find-file (universal-argument-of-this-command)))))

(define-command fermin/up-directory () ()
  "Delete the last path segment in file prompt."
  (alexandria-2:when-let* ((pwindow (lem/prompt-window::current-prompt-window))
                           (wstring (and pwindow (lem/prompt-window::get-input-string))))
    (lem/prompt-window::replace-prompt-input
     (ignore-errors
       (let* ((trimmed (str:trim-right wstring :char-bag '(#\/ )))
              (endp (1+ (position #\/ trimmed :from-end t :test #'char-equal))))
         (subseq trimmed 0 endp))))))

