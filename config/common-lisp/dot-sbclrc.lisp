;;;; dot-sbclrc.lisp -> .sbclrc - SBCL Initialization File

;;; The following lines added by ql:add-to-init-file:
#-quicklisp
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp"
                                       (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

;; Ref https://www.reddit.com/r/GUIX/comments/10ju937/quicklisp_on_guix/
;; Ref https://lists.gnu.org/archive/html/bug-guix/2020-01/msg00133.html
;; cffi doesn't care about LIBRARY_PATH, but this is where Guix adds
;; all libraries. We forward these directories to cffi.
;; Comment out for now -> not helping with qlot issue...
;; (ql:quickload :cffi)
;; (dolist (dir (uiop:getenv-pathnames "LIBRARY_PATH"))
;;   ;; The path need / at the end, so ensure this
;;   (pushnew (uiop:ensure-directory-pathname dir)
;;            cffi:*foreign-library-directories*
;;            :test #'equal))

;;; The Guix Way...
;; Ref: https://github.com/atlas-engineer/nyxt/blob/master/documents/
;;      README.org#developers-installation-guide
;; (require "asdf")

;; (let ((guix-profile (format nil "~a/.guix-profile/lib/" (uiop:getenv "HOME"))))
;;   (when (and (probe-file guix-profile)
;;              (ignore-errors (asdf:load-system "cffi")))
;;     (push guix-profile
;;           (symbol-value (find-symbol (string '*foreign-library-directories*)
;;                                      (find-package 'cffi))))))
