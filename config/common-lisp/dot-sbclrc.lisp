;;;; dot-sbclrc.lisp -> .sbclrc - SBCL Initialization File

;;; ASDF Registry
;; You can drop Common Lisp projects into any of these folders:
;; |-> ~/quicklisp/local-projects
;; |-> ~/common-lisp,
;; |-> ~/.local/share/common-lisp/source,
;; A library installed here is automatically available for every project.

;;; The following lines added by ql:add-to-init-file:
#-quicklisp
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp"
                                       (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

;;; The Guix Way...
;;; Adapt to work with my current setup...
;; Ref: https://github.com/atlas-engineer/nyxt/blob/master/documents/
;;      README.org#developers-installation-guide
;; (require "asdf")
;; (let ((guix-profile (format nil "~a/.guix-profile/lib/" (uiop:getenv "HOME"))))
;;   (when (and (probe-file guix-profile)
;;              (ignore-errors (asdf:load-system "cffi")))
;;     (push guix-profile
;;           (symbol-value (find-symbol (string '*foreign-library-directories*)
;;                                      (find-package 'cffi))))))
