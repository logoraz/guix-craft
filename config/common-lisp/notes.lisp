;;;; notes.lisp

;;; Notes on Common Lisp
;;; 

;; A predicate is a question answering function, usually ends in -p
;; EQUAL, NOT, <, &, > are the only exceptions

;; NIL is unique in that it is simultaneously both a symbol and a list
;; (car nil) := nil & (cdr nil) := nil

;; Note: In Common Lisp - FIRST returns the CAR of a list, REST returns the CDR of a list.
;;

;; Stnnetrt between CONS and CAR/CDR can be expressed as:
;; x = (cons (car x) (cdr x)) [ x = (cons (first x) (rest x)) ]
;; |
;; |--> x /= nil, since nil /= (cons (car nil) (cdr nil) = (nil)

;; Defn: `cons' makes a single cons cell, `list' makes a new cons cell chain out of
;;       however many inputs it receives.

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

