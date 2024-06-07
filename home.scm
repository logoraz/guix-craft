;;;; Guix HOME Configuration

(use-modules (gnu home)
             (gnu home services)
             (gnu home services desktop)
             (gnu home services sound)
             (gnu home services shells)
             (gnu packages)
             (gnu services)
             (guix gexp))


(define %logoraz-packages
  (list
   ;; Fonts
   "font-fira-code"
   "font-fira-go"
   "font-dejavu"
   "font-iosevka-aile"
   "font-sarasa-gothic"
   "font-apple-sf-pro"
   "font-apple-sf-symbols"
   "font-apple-sf-arabic"
   "font-google-noto"
   "font-google-noto-sans-cjk"
   "font-google-material-design-icons"
   ;; WWW/Mail
   ;; "nyxt" ;-> Disable for now, will install for development purposes!
   "icecat"
   "keepassxc"
   "wl-clipboard"
   "gstreamer"
   "gst-plugins-good"
   "gst-plugins-bad"
   "gst-libav"
   "isync"
   "msmtp"
   "mu"
   ;; Documents/Files -> disabled for now (these are huge)
   ;; "texlive-scheme-basic"
   ;; "texlive-collection-latexrecommended"
   ;; "texlive-collection-fontsrecommended"
   ;; Apps
   "gnucash"
   "gimp"
   "inkscape"
   "blender"
   "mpv"
   "vlc"
   "foot"))

(define %dev-packages  ;-> perhpas move to a manifest
  (list
   ;; Guile Dev Tools
   "guile-next"        ; needed for ares/arei
   "guile-ares-rs"     ; for mREPL Guile Scheme Emacs IDE
   ;; Common Lisp GUI Dev
   "sbcl"                      ; Steel Bank Common Lisp (Primary)
   ;; "clasp"                     ; CLASP Common Lisp (Secondary)
   "ccl"                       ; Clozure Common Lisp (Tertiary)
   ;; Common Lisp Tools
   ;;
   ;; Base Dev Tools
   "make"
   "binutils"
   "tar"
   "gzip"
   "zip"
   "unzip"
   "openssl"
   "curl"
   "gnupg"
   "git"
   "git:send-email"))

(define %emacs-packages ;-> perhaps move to a manifest
  (list
   "emacs-next-pgtk"
   "emacs-diminish"
   "emacs-delight"
   "emacs-nerd-icons"
   "emacs-doom-themes"
   "emacs-doom-modeline"
   "emacs-ligature"
   "emacs-no-littering"
   "emacs-ws-butler"
   "emacs-undo-tree"
   "emacs-paredit"
   "emacs-visual-fill-column"
   "emacs-mct"
   "emacs-orderless"
   "emacs-corfu"
   "emacs-marginalia"
   "emacs-beframe"
   "emacs-denote"
   "emacs-magit"
   "emacs-vterm"
   "emacs-guix"
   "emacs-arei"
   "emacs-sly"
   "emacs-mbsync"
   "emacs-org-superstar"
   "emacs-org-appear"
   "emacs-erc-hl-nicks"
   "emacs-erc-image"
   "emacs-emojify"
   "emacs-bongo"))

(home-environment
 ;; Below is the list of packages that will show up in your
 ;; Home profile, under ~/.guix-home/profile.
 (packages (specifications->packages
            (append
             %logoraz-packages
             %dev-packages
             %emacs-packages)))
 
 ;; Below is the list of Home services.  To search for available
 ;; services, run 'guix home search KEYWORD' in a terminal.
 (services
  (list
   (simple-service 'env-vars home-environment-variables-service-type
                   '(("EDITOR" . "emacs")
                     ("BROWSER" . "nyxt")))
   (service home-bash-service-type
            (home-bash-configuration
	     (guix-defaults? #f)
             (aliases '(("grep" . "grep --color=auto")
                        ("ls"   . "ls -p --color=auto")
                        ("ll"   . "ls -l")
                        ("la"   . "ls -la")
                        ("ghr"  . "guix home reconfigure")
                        ("gsr"  . "sudo guix system reconfigure")
                        ("gup"  . "guix pull && guix upgrade")
                        ("gud"  . "sudo guix system delete-generations")
                        ("ghd"  . "guix home delete-generations")))
             (bashrc
              (list (local-file "./config/dot-bashrc.sh"
                                #:recursive? #t)))
             (bash-profile
              (list (local-file "./config/dot-bash_profile.sh"
                                #:recursive? #t))))))))
