(use-modules (guix packages)
             (guix git-download)
             (guix build-system asdf)
             (guix build-system copy)
             ((guix licenses) #:prefix license:)
             (guix gexp)
             (gnu packages base)
             (gnu packages tls)
             (gnu packages version-control)
             (gnu packages gnupg)
             (gnu packages lisp)
             (gnu packages commencement)
             (gnu packages lisp-check)
             (gnu packages lisp-xyz)
             (git bindings)
             (git describe)
             (gnu packages version-control))

(define (git-checkout-description checkout)
  (libgit2-init!)
  (let-values (((commit description) (describe-checkout checkout)))
    description))

(define sbcl-iconv
  (let ((commit "54900c3f00e19da15a9c65451bddde839d0a7f75")
        (revision "0"))
    (package
      (name "sbcl-iconv")
      (version (git-version "0.3" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/quek/cl-iconv")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "1lpw95c02inifhdh9kkab9q92i5w9zd788dww1wly2p0a6kyx9wg"))))
      (build-system asdf-build-system/sbcl)
      (inputs (list sbcl-cffi libiconv))
      (home-page "https://github.com/quek/cl-iconv")
      (synopsis "iconv(man 3 iconv) library for Common Lisp")
      (description
       "This package provides CFFI bindings to convert between different
character encodings using iconv.")
      (license license:expat))))

(define sbcl-cl-setlocale
  (let ((commit "f660d07dac72bc3e99caae1c6c8a789991e2694c")
        (revision "0"))
    (package
      (name "sbcl-cl-setlocale")
      (version (git-version "1.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/shamazmazum/cl-setlocale")
               (commit commit)))
         (file-name (git-file-name "cl-setlocale" version))
         (sha256
          (base32 "0g1b89yj6n42ayf2074krk3h9yvglqxn54a6i3sxgpsqww2ll2a1"))))
      (build-system asdf-build-system/sbcl)
      (inputs (list sbcl-cffi))
      (native-inputs (list sbcl-fiveam))
      (home-page "https://github.com/shamazmazum/cl-setlocale")
      (synopsis "Wrapper around setlocale(3) usable with other CFFI libraries")
      (description
       "This library is a tiny wrapper around setlocale(3)
and can be used in conjunction with other FFI wrappers like cl-charms.")
      (license license:bsd-2))))

(define sbcl-lem-mailbox
  (let ((commit "12d629541da440fadf771b0225a051ae65fa342a")
        (revision "1"))
    (package
      (name "sbcl-lem-mailbox")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/lem-project/lem-mailbox")
               (commit commit)))
         (sha256
          (base32 "1qh9yq9ks0paplmbx0vj4nynx86igkv9kli396plpg9vc14qdgl5"))
         (file-name (git-file-name "cl-lem-mailbox" version))))
      (build-system asdf-build-system/sbcl)
      (inputs (list sbcl-bordeaux-threads sbcl-bt-semaphore sbcl-queues))
      (native-inputs (list sbcl-rove))
      (synopsis "ANSI CL adaptation of the SBCL mailbox utility")
      (description "ANSI CL adaptation of the SBCL mailbox utilty.
Tested on ABCL, but should work on any implementation.")
      (home-page "https://github.com/lem-project/lem-mailbox")
      (license license:expat))))

(define-public sbcl-micros
  (let ((commit "23f52d5349382d3d50c855b75a665f3158286390")
        (revision "1"))
    (package
      (name "sbcl-micros")
      (version (git-version "1.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/lem-project/micros")
               (commit commit)))
         (sha256
          (base32 "13hwx7swlibk9wbix1jfjw23bmwpjq46lh405386w8l95p5ga322"))
         (file-name (git-file-name "micros" version))
         (snippet #~(begin
                      (use-modules (guix build utils))
                      (substitute* "lsp-api.lisp"
                        (("ql:quickload")
                         "asdf:load-systems"))))))
      (build-system asdf-build-system/sbcl)
      (native-inputs (list sbcl-rove))
      (synopsis "SLIME/SWANK implementation for the Lem editor")
      (description
       "Micros is a SLIME/SWANK implementation meant for use by
the Lem editor for Common Lisp. Breaking changes in SLIME/SWANK
led Lem to maintain it's own fork to ease maintainance burden.")
      (home-page "https://github.com/lem-project/micros")
      (license license:expat))))

(define sbcl-async-process
  (let ((commit "9690530fc92b59636d9f17d821afa7697e7c8ca4")
        (revision "0"))
    (package
      (name "sbcl-async-process")
      (version (git-version "0.0.1" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/lem-project/async-process")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "1m2sfgfg6c0gqqy1pqsahsiw3j25y473mfw7sx0akkqbhwhm7mjb"))))
      (build-system asdf-build-system/sbcl)
      (inputs (list sbcl-cffi))
      (home-page "https://github.com/lem-project/async-process")
      (synopsis "Asynchronous process execution for Common Lisp")
      (description "This library provides an asynchronous process
execution mechanism for Common Lisp.")
      (license license:expat))))

(define sbcl-bt-semaphore
  (let ((commit "46b4bf315590f510d2d4ec5ca8908efbe68007e9")
        (revision "1"))
    (package
      (name "sbcl-bt-semaphore")
      (version (git-version "0.6.2" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/r-moeritz/bt-semaphore")
               (commit commit)))
         (sha256
          (base32 "0rl7yp36225z975hg069pywwlpchwn4086cgxwsi2db5mhghpr7l"))
         (file-name (git-file-name "cl-bt-semaphore" version))))
      (build-system asdf-build-system/sbcl)
      (inputs (list sbcl-bordeaux-threads))
      (native-inputs (list sbcl-clunit))
      (synopsis "Semaphore implementation for @code{bordeaux-threads}")
      (description
       "@code{bt-semaphore} is a semaphore implementation for use with
@code{bordeaux-threads}.

Since version 0.8.6, @code{bordeaux-threads}
supplies its own built-in semaphores. It is reccomended to use those instead.")
      (home-page "https://github.com/r-moeritz/bt-semaphore")
      (license license:expat))))

(define-public sbcl-jsonrpc
  (let ((commit "035ba8a8f2e9b9968786ee56b59c7f8afbea9ca2")
        (revision "1"))
    (package
      (name "sbcl-jsonrpc")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/cxxxr/jsonrpc")
               (commit commit)))
         (file-name (git-file-name "jsonrpc" version))
         (sha256
          (base32 "00axmjd81j4pf1pmvwbkcyr5lr9v7n16zqnazpf5vp86x4lvp0yy"))))
      (build-system asdf-build-system/sbcl)
      (native-inputs (list sbcl-rove))
      (inputs (list sbcl-clack
                    sbcl-http-body
                    sbcl-lack
                    sbcl-yason
                    sbcl-bordeaux-threads
                    sbcl-event-emitter
                    sbcl-alexandria
                    sbcl-dissect
                    sbcl-chanl
                    sbcl-vom
                    sbcl-usocket
                    sbcl-websocket-driver))
      (home-page "https://github.com/cxxxr/jsonrpc")
      (synopsis "JSON-RPC 2.0 server/client for Common Lisp")
      (description "This package provides a JSON-RPC 2.0
server/client for Common Lisp.")
      (license license:bsd-2))))

(define-public sbcl-inquisitor
  (let ((commit "423fa9bdd4a68a6ae517b18406d81491409ccae8")
        (revision "1"))
    (package
      (name "sbcl-inquisitor")
      (version (git-version "0.5" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/t-sin/inquisitor/")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "08rkmqnwlq6v84wcz9yp31j5lxrsy33kv3dh7n3ccsg4kc54slzw"))))
      (build-system asdf-build-system/sbcl)
      (native-inputs (list sbcl-prove sbcl-babel))
      (inputs (list sbcl-flexi-streams sbcl-alexandria sbcl-anaphora))
      (home-page "https://github.com/t-sin/inquisitor")
      (synopsis
       "Encoding/end-of-line detection and external-format abstraction for Common Lisp")
      (description
       "Inquisitor is a cross-implementation library provding
encoding/end-of-line detection and external-format abstraction for Common Lisp.")
      (license license:expat))))

(define lem-base16-themes
  ;; keep private
  (let ((commit "07dacae6c1807beaeffc730063b54487d5c82eb0")
        (revision "0"))
    (package
      (name "lem-base16-themes")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/lem-project/lem-base16-themes")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "0hqscypvp5rd8qiwwqh46lip0rjg4bpggjf7sjff7qxgimylk1aj"))))
      (build-system asdf-build-system/source)
      (home-page "https://github.com/lem-project/lem-base16-themes")
      (synopsis "Themes for Lem editor")
      (description "Themes for Lem editor.")
      (license license:expat))))

(define-public sbcl-lisp-preprocessor
  (let ((commit "cbed5952f3d98c84448c52d12255df9580451383")
        (revision "0"))
    (package
      (name "sbcl-lisp-preprocessor")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/cxxxr/lisp-preprocessor")
               (commit commit)))
         (file-name (git-file-name "lisp-preprocessor" version))
         (sha256
          (base32 "0v0qhawcvgbxk06nfwyvcqwmqvzn2svq80l2rb12myr0znschhpi"))))
      (build-system asdf-build-system/sbcl)
      (native-inputs (list sbcl-rove))
      (inputs (list sbcl-alexandria
                    sbcl-split-sequence
                    sbcl-trivial-gray-streams
                    sbcl-trivia
                    sbcl-cl-ppcre))
      (home-page "https://github.com/cxxxr/lisp-preprocessor")
      (synopsis "Common Lisp embedded template engine.")
      (description "Common Lisp embedded template engine.")
      (license license:expat))))

(define-public sbcl-trivial-ws
(let ((commit "ebf1ec0ea26bdac4007e98e89f3a621dbfb4390a")
      (revision "0"))
    (package
      (name "sbcl-trivial-ws")
      (version (git-version "0.1.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/ceramic/trivial-ws")
               (commit commit)))
         (file-name (git-file-name "trivial-ws" version))
         (sha256
          (base32 "0qmsf0dhmyhjgqjzdgj2yb1nkrijwp4p1j411613i45xjc2zd6m7"))))
      (build-system asdf-build-system/sbcl)
      (inputs (list sbcl-hunchensocket
                    ;; trivial-ws-client
                    sbcl-websocket-driver
                    sbcl-cl-async
                    ;; trivial-ws-test
                    sbcl-prove
                    sbcl-find-port))
      (home-page "https://github.com/ceramic/trivial-ws")
      (synopsis "Trivial WebSockets, built on top of Hunchensocket for the server and websocket-driver for the client.")
      (description "Trivial WebSockets, built on top of Hunchensocket for the server and websocket-driver for the client.")
      (license license:expat))))

(define-public sbcl-hunchensocket
(let ((commit "faf2c08452f18763e541bc7f121760669ac0f41a")
      (revision "1"))
    (package
      (name "sbcl-hunchensocket")
      (version (git-version "1.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/joaotavora/hunchensocket")
               (commit commit)))
         (file-name (git-file-name "hunchensocket" version))
         (sha256
          (base32 "1vhd009lwl62l1czmhsalblxmyz4x9v3nspjflpajwm1db5rnd7h"))))
      (build-system asdf-build-system/sbcl)
      (native-inputs (list sbcl-fiasco))
      (inputs (list sbcl-hunchentoot
                    sbcl-alexandria
                    sbcl-cl-base64
                    sbcl-sha1
                    sbcl-flexi-streams
                    sbcl-chunga
                    sbcl-trivial-utf-8
                    sbcl-trivial-backtrace
                    sbcl-bordeaux-threads
                    sbcl-cl-fad))
      (home-page "https://github.com/joaotavora/hunchensocket")
      (synopsis "Hunchensocket is a Common Lisp implementation of WebSockets realized as an extension to the Hunchentoot web server.")
      (description "Hunchensocket is a Common Lisp implementation of WebSockets realized as an extension to the Hunchentoot web server.")
      (license license:bsd-2))))

(define lem
  (let ((commit "f6333ee3908afc96c22987bc76de4773e56da2b6")
        (revision "0"))
    (package
     (name "lem")
     (version (git-version "2.2.0" revision commit))
     ;; (source (local-file (dirname (dirname (current-filename))) #:recursive? #t))
     (source
      (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/lem-project/lem/")
             (commit commit)))
       (sha256
        (base32 "1nan7scvx7j6gdagdi4k3k8jn1qbb3fbja35rfhz684la7a7jnal"))
       (file-name (git-file-name name version))
       (snippet #~(begin
                    (use-modules (guix build utils))
                    ;; delete roswell-specific files since they aren't needed
                    (delete-file-recursively "roswell")))))
     (build-system asdf-build-system/sbcl)
     (outputs '("out" "ncurses"))
     (arguments
      '(#:asd-systems '("lem-sdl2/executable" "lem-ncurses")
        #:phases (modify-phases %standard-phases
                                (add-after 'unpack 'override-ql
                                           (lambda* _
                                             (substitute* (find-files (getcwd) "\\.lisp$")
                                                          (("ql:quickload")
                                                           "asdf:load-systems"))))
                                (add-after 'override-ql 'redirect-home
                                           (lambda _
                                             (setenv "HOME" "/tmp")))
                                (add-after 'create-asdf-configuration 'build-program
                                           (lambda* (#:key outputs #:allow-other-keys)
                                             (begin
                                               (build-program (string-append (assoc-ref outputs
                                                                                        "out")
                                                                             "/bin/lem")
                                                              outputs
                                                              #:dependencies '("lem-sdl2")
                                                              #:entry-program '((lem:main)
                                                                                0))
                                               (build-program (string-append (assoc-ref outputs
                                                                                        "ncurses")
                                                                             "/bin/lem")
                                                              outputs
                                                              #:dependencies '("lem-ncurses")
                                                              #:entry-program '((lem:main)
                                                                                0))))))))
     (inputs (list
              ;; lem.asd
              sbcl-alexandria
              sbcl-trivia
              sbcl-trivial-gray-streams
              sbcl-trivial-types
              sbcl-cl-ppcre
              sbcl-closer-mop
              sbcl-iterate
              sbcl-micros
              sbcl-lem-mailbox
              sbcl-inquisitor
              sbcl-babel
              sbcl-bordeaux-threads
              sbcl-yason
              sbcl-log4cl
              sbcl-split-sequence
              sbcl-cl-str
              sbcl-dexador
              ;; lem-sdl2
              sbcl-sdl2
              sbcl-sdl2-ttf
              sbcl-sdl2-image
              sbcl-trivial-main-thread
              ;; lem-ncurses
              sbcl-cffi
              sbcl-cl-charms
              sbcl-cl-setlocale
              ;; lem-language-server
              sbcl-log4cl
              sbcl-jsonrpc
              sbcl-usocket
              sbcl-quri
              sbcl-cl-change-case
              sbcl-async-process
              ;; lem-encodings-table
              sbcl-iconv
              ;; lem-vi-mode
              sbcl-esrap
              sbcl-parse-number
              sbcl-cl-package-locks
              ;; lem-scheme-mode
              sbcl-slime-swank
              ;; lem-jsonrpc
              sbcl-trivial-utf-8
              lem-base16-themes
              ;; lem-markdown-mode
              sbcl-3bmd
              sbcl-lisp-preprocessor
              sbcl-trivial-open-browser
              sbcl-trivial-ws))
     (native-inputs (list sbcl-rove))
     (synopsis "Integrated IDE/editor for Common Lisp")
     (description "TODO")
     (home-page "http://lem-project.github.io/")
     (license license:expat))))
lem
