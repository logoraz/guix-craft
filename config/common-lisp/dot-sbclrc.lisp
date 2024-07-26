;;;; dot-sbclrc.lisp -> .sbclrc - SBCL Initialization File

;;; Configure SBCL to add CFFI's to GUIX profile for ASDF to find?
;;; Ref: https://github.com/atlas-engineer/nyxt/blob/master/documents/
;;;      README.org#developers-installation-guide
;;; (require "asdf")
;;; (let ((guix-profile (format nil "~a/.guix-profile/lib/" (uiop:getenv "HOME"))))
;;;   (when (and (probe-file guix-profile)
;;;              (ignore-errors (asdf:load-system "cffi")))
;;;     (push guix-profile
;;;           (symbol-value (find-symbol (string '*foreign-library-directories*)
;;;                                      (find-package 'cffi))))))
