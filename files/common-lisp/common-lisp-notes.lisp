;;;; notes.lisp |--> Common Lisp

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
;; ref: https://stackoverflow.com/questions/60247877/check-for-proper-list-in-common-lisp
(defun proper-list-p (x)
  (not (null (handler-case (list-length x) (type-error () nil)))))

;;; Unary Arithmetic with Lists
;; List may be used to do unary ("base one") arithmetic.
;; REST --> subtraction (substracts 1), CDDR substracts 2
;; LENGTH --> translates unary into integers
;; NULL --> acts as the ZEROP predicate
;; FIRST/CAR --> acts as the PLUSP predicate

;; Nonlist cons structures
;; Defn: Proper List --> ends in NIL
;; Defn: Dotted List --> A list that does not end in NIL, can only be made with CONS
;; Defn: Dotted Pair --> A single cons cell whose CDR/REST is not NIL.

;;; Circular Lists
;; Common Lisp provides "sharp-equal notation" for representing circular lists
;; #1=(a b c . #1)
(defparameter *circular-list* (list 'a 'b 'c))

(setf *print-circle* t)

(nconc *circular-list* *circular-list*)
;; => #1=(A B C . #1)

(third *circular-list*)

;; Prove by contradiction that this list cannot be constructed using just CONS cells.

;; #1=[*|*]--->[*|*]--->[*|*]--->#1#
;;     |        |        |
;;     v        v        v
;;     A        B        C

;; The list #1=(a b c . #1) can be constructed by CONS cells
;; Since the cons cell of A points to the cons cell of B, this means that the cons cell of B
;; was created first. Likewise, since the cons cell of B points to the cons cell of C,
;; then the cons cell of C was created before consB and consA:
;; consC <-- consB <-- consA
;; However, if the cons cell of C points to the cons cell of A, then the cons cell of
;; A would have had to be constructed before the cons cell of C, which contradicts that
;; the cons cell of C was the first cons cell constructed, prior to cB and cA.

;;; Eval Notation
;; Allows representing functions as ordinary lists, which allows the exact same notation
;; for both functions and data. This also allows us to represent functions that accept
;; other functions as inputs.
;;
;;; The EVAL function --> the heart of listp
;; Numbers, NIL, and T --> self-evaluating, i.e. they evaluate to themselves.

