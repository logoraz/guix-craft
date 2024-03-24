;; home-alist.scm
;; Borroewed from iambumblehead
;;

(use-modules (guix gexp)
             (srfi srfi-1)
             (srfi srfi-26)
             (ice-9 ftw)
             (ice-9 string-fun)
             (ice-9 regex))

(define %current-dir (dirname (current-filename)))
(define isrootre (make-regexp "^\\/"))
(define isdotre (make-regexp "^\\.\\.?"))
(define istmpre (make-regexp "~$"))

(define (path-join . args)
  (string-join args file-name-separator-string))

(define (path-expand path)
  (if (regexp-exec isrootre path)
      path (path-join %current-dir path)))

(define (path-diff path-sub path-full)
  "(/full/path/ /full/path/to/file.cfg) -> to/file.cfg"
  (let ((path-sub-and-slash
         (string-append (dirname (path-expand path-sub))
                        file-name-separator-string)))
    (string-replace-substring path-full path-sub-and-slash "")))

(define (isnotdotortmpstr? str)
  (and (not (regexp-exec isdotre str))
       (not (regexp-exec istmpre str))))

(define (list-recursive pathOrDir . files)
  (let ((filestat (stat pathOrDir)))
    (cond ((eq? (stat:type filestat) 'directory)
           (fold (lambda (str prev)
                   (append prev
                           (list-recursive
                            (path-join pathOrDir str))))
                 files
                 (scandir pathOrDir isnotdotortmpstr?)))
          ((eq? (stat:type filestat) 'regular)
           (cons pathOrDir files))
          (else files))))

(define (home-list-create-from-dir source home-lists)
  (let ((files (list-recursive source)))
    (fold (lambda (path-to prev)
            (let ((path-full (path-expand path-to)))
              (cons (list (path-diff source path-full)
                          (local-file path-full #:recursive? #t))
                    prev)))
          home-lists
          files)))

(define (home-alists-create-from-dirs sources . alists)
  (fold (lambda (source prev)
          (home-list-create-from-dir source prev))
        alists
        sources))
