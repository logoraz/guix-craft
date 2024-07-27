;;;; dev-tools-manifest.scm
;;; To be used with guix shell command as follows
;;;
;;; guix shell -m devtools-manifest.scm
;;;
;;; or can invoke with a specific program
;;;
;;; guix shell -m devtools-manifest.scm -- emacs
;;;

(specifications->manifest
 '( ;; "libtree"
   "gcc-toolchain"
   "binutils"
   "make"
   "curl"
   "ccl"))
