;;;; notes.lisp

;;; Notes on Common Lisp
;;; 

;; A predicate is a question answering function, usually ends in -p
;; EQUAL, NOT, <, &, > are the only exceptions

;; NIL is unique in that it is simultaneously both a symbol and a list
;; (car nil) := nil & (cdr nil) := nil

;; Stnnetrt between CONS and CAR/CDR can be expressed as:
;; x = (cons (car x) (cdr x)) [ x = (cons (first x) (rest x)) ]
;; |
;; |--> x /= nil, since nil /= (cons (car nil) (cdr nil) = (nil)

;; Note: In Common Lisp - FIRST returns the CAR of a list, REST returns the CDR of a list.
;;

;; Defining XOR function (simple):
(defun xor (x y)
  "Simple implementation of XOR."
  (not (equal x y)))

;;;; Replace First Element of a list
(defun replace-first (elem list)
  "Replace the first element of a list with ELEM."
  (cons elem (rest list)))

;; NOT = NULL (predicates), NOT is reserved for logical operations and NULL for list operations.
;; LENGTH fails for non-nil terminated lists

;; listp and consp (is this equal to scheme's pair?)
;; In Common Lisp (pairp '(a . b)) => T, whereas in Scheme (pair? '(a . b)) => #f
(defun proper-listp (list)
  "Predicate to test for proper list, i.e. nil terminated."
  (loop for item in list))

;;; Lem Stuff
(defun connected-slime-message (connection)
  (display-popup-message
   (format nil "Swank server running on ~A ~A"
           (connection-implementation-name connection)
           (connection-implementation-version connection))
   :timeout 1
   :style '(:gravity :center)))

(defun connect-to-micros (hostname port)
  (let ((connection
          (handler-case (if (eq hostname *localhost*)
                            (or (ignore-errors (new-connection "127.0.0.1" port))
                                (new-connection "localhost" port))
                            (new-connection hostname port))
            (error (c)
              (editor-error "~A" c)))))
    (add-and-change-connection connection)
    (start-thread)
    connection))

(define-command slime-connect (hostname port &optional (start-repl t))
    ((:splice
      (list (prompt-for-string "Hostname: " :initial-value *localhost*)
            (parse-integer
             (prompt-for-string "Port: "
                                :initial-value (princ-to-string *default-port*))))))
  (let ((connection (connect-to-micros hostname port)))
    (when start-repl (start-lisp-repl))
    (connected-slime-message connection)))
