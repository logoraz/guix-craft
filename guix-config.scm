;;;; Guix OS - System & Home Configuration

(use-modules (gnu)
	     (gnu packages)
             (gnu services)
             (gnu home)
             (gnu home services)
             (gnu home services desktop)
             (gnu home services sound)
             (gnu home services shells)
             (gnu home services dotfiles)
	     (guix packages)
             (guix channels)
	     (guix download)
             (guix gexp)
             (guix transformations)
             (guix ci)
	     (nongnu packages linux)
             (nongnu system linux-initrd))

(use-system-modules keyboard)

(use-package-modules lisp lisp-xyz wm xorg xdisorg linux fonts freedesktop
                     ssh cups suckless networking package-management
                     fonts web-browsers gnuzilla password-utils gnupg mail
                     gstreamer video compton image-viewers linux music
                     gnucash gimp inkscape graphics compression version-control
                     guile guile-xyz emacs emacs-xyz text-editors)

(use-service-modules guix cups ssh desktop xorg)

(primitive-load "home-impure-symlinks.scm")

;;; Define Channels
(define guix-channel
  (channel
   (name 'guix)
   (url "https://git.savannah.gnu.org/git/guix.git")
   (introduction
    (make-channel-introduction
     "9edb3f66fd807b096b48283debdcddccfea34bad"
     (openpgp-fingerprint
      "BBB0 2DDF 2CEA F6A8 0D1D  E643 A2A0 6DF2 A33A 54FA")))))

(define nonguix-channel
  (channel
   (name 'nonguix)
   (url "https://gitlab.com/nonguix/nonguix")
   (introduction
    (make-channel-introduction
     "897c1a470da759236cc11798f4e0a5f7d4d59fbc"
     (openpgp-fingerprint
      "2A39 3FFF 68F4 EF7A 3D29  12AF 6F51 20A0 22FB B2D5")))))

(define flatwhatson-channel
  (channel
   (name 'flat)
   (url "https://github.com/flatwhatson/guix-channel.git")
   (introduction
    (make-channel-introduction
     "33f86a4b48205c0dc19d7c036c85393f0766f806"
     (openpgp-fingerprint
      "736A C00E 1254 378B A982  7AF6 9DBE 8265 81B6 4490")))))

(define guixrus-channel
  (channel
   (name 'guixrus)
   (url "https://git.sr.ht/~whereiseveryone/guixrus")
   (introduction
    (make-channel-introduction
     "7c67c3a9f299517bfc4ce8235628657898dd26b2"
     (openpgp-fingerprint
      "CD2D 5EAA A98C CB37 DA91  D6B0 5F58 1664 7F8B E551")))))

(define rde-channel
  (channel
  (name 'rde)
  (url "https://git.sr.ht/~abcdw/rde")
  (branch "master")
  (introduction
    (make-channel-introduction
      "257cebd587b66e4d865b3537a9a88cccd7107c95"
      (openpgp-fingerprint
        "2841 9AC6 5038 7440 C7E9  2FFA 2208 D209 58C1 DEB0")))))

(define aadcg-channel
  (channel
   (name 'aadcg)
   (url "https://github.com/aadcg/aadcg-guix-channel")))

;; TODO - Consider creating my own channel:
;; Ref: https://guix.gnu.org/cookbook/en/html_node/Channels.html
;; https://guix.gnu.org/manual/en/html_node/Creating-a-Channel.html#Creating-a-Channel
(define logoraz-channels
  (list nonguix-channel
        guix-channel))
  ;; (append (list nonguix-channel
  ;;               guix-channel)
  ;;         %default-channels))

;;; Home Package Transformations
;; nyxt --> (latest-nyxt nyxt)
(define latest-nyxt
  (options->transformation
   '((without-tests . "nyxt")
     (with-latest   . "nyxt"))))

;;; Home User Configuration Definitions & Services
(define logoraz-packages
  (list font-fira-code ;;|--> gnu packages fonts
        font-iosevka-aile
        font-google-noto
        font-google-noto-emoji
        font-google-noto-sans-cjk
        lem       ;;|--> gnu packages text-editors
        (latest-nyxt nyxt) ;;|--> gnu packages web-browsers :www-mail
        icecat    ;;|--> gnu packages gnuzilla
        keepassxc ;;|--> gnu packages password-utils
        gnupg     ;;|--> gnu packages gnupg
        isync     ;;|--> gnu packages mail
        msmtp
        mu
        gstreamer ;;|--> gnu packages gstreamer
        gst-plugins-good
        gst-plugins-bad
        gst-libav
        mpv ;;|--> gnu packages video :apps
        vlc
        picom    ;;|--> gnu packages compton
        feh      ;;|--> gnu packages image-viewers
        pipewire ;;|--> gnu packages linux
        wireplumber
        lm-sensors
        brightnessctl
        playerctl ;;|--> gnu packages music
        gnucash   ;;|--> gnu packages gnucash
        gimp      ;;|--> gnu packages gimp
        inkscape  ;;|--> gnu packages inkscape
        blender   ;;|--> gnu packages graphics
        git))     ;;|--> gnu packages version-control

(define emacs-packages
  (list  guile-next      ;;|--> gnu packages guile
         guile-ares-rs   ;;|--> gnu packages guile-xyz
         emacs           ;;|--> gnu packages emacs
         emacs-diminish  ;;|--> gnu packages emacs-xyz
         emacs-delight
         emacs-nord-theme
         emacs-doom-themes
         emacs-nerd-icons
         emacs-doom-modeline
         emacs-ligature
         emacs-no-littering
         emacs-ws-butler
         emacs-undo-tree
         emacs-paredit
         emacs-visual-fill-column
         emacs-ace-window
         emacs-mct
         emacs-orderless
         emacs-corfu
         emacs-marginalia
         emacs-beframe
         emacs-denote
         emacs-magit
         emacs-vterm
         emacs-guix
         emacs-arei
         emacs-sly
         emacs-mbsync
         emacs-org-superstar
         emacs-org-appear
         emacs-erc-hl-nicks
         emacs-erc-image
         emacs-emojify))

(define *home-path* "/home/logoraz/repos/guix-craft/")

(define logoraz-home
  (home-environment
   ;; Below is the list of packages that will show up in your
   ;; Home profile, under ~/.guix-home/profile.
   (packages (append
              logoraz-packages
              emacs-packages))

   ;; Below is the list of Home services.  To search for available
   ;; services, run 'guix home search KEYWORD' in a terminal.
   (services
    (list
     (service home-pipewire-service-type)
     (service home-dbus-service-type) ;; for bluetooth --> system
     (simple-service 'home-impure-symlinks-dotfiles
                     home-impure-symlinks-service-type
                     `(;; StumpWM XDG Configuration Scaffolding
                       (".config/stumpwm/config"
                        ,(string-append
                          *home-path*
                          "config/stumpwm/config.lisp"))
                       (".config/stumpwm/libraries"
                        ,(string-append
                          *home-path*
                          "config/stumpwm/libraries"))
                       (".config/stumpwm/modules"
                        ,(string-append
                          *home-path*
                          "config/stumpwm/modules"))
                       ;; Xorg Configuration Scaffolding
                       (".Xdefaults"
                        ,(string-append
                          *home-path*
                          "config/xorg/dot-Xdefaults"))
                       (".Xresources"
                        ,(string-append
                          *home-path*
                          "config/xorg/dot-Xresources"))
                       (".icons"
                        ,(string-append
                          *home-path*
                          "config/xorg/dot-icons"))
                       (".config/xorg/start-xterm.sh"
                        ,(string-append
                          *home-path*
                          "config/xorg/start-xterm.sh"))
                       ;; Common Lisp Configration Scaffolding
                       (".config/common-lisp/source-registry.conf.d"
                        ,(string-append
                          *home-path*
                          "config/common-lisp/source-registry.conf.d"))
                       (".sbclrc"
                        ,(string-append
                          *home-path*
                          "config/common-lisp/dot-sbclrc.lisp"))
                       (".clasprc"
                        ,(string-append
                          *home-path*
                          "config/common-lisp/dot-clasprc.lisp"))
                       ;; Emacs Configuration Scaffolding
                       (".config/emacs"
                        ,(string-append
                          *home-path*
                          "config/emacs"))
                       ;; Lem Configuration Scaffolding
                       (".config/lem"
                        ,(string-append
                          *home-path*
                          "config/lem"))
                       ;; Nyxt Configuration Scaffolding
                       (".config/nyxt"
                        ,(string-append
                          *home-path*
                          "config/nyxt"))
                       (".local/share/nyxt/extensions"
                        ,(string-append
                          *home-path*
                          "config/nyxt/extensions"))))
     (simple-service 'env-vars home-environment-variables-service-type
                     '(("EDITOR" . "emacs")
                       ("BROWSER" . "nyxt")
                       ;; ("OPENER" . "opener.sh")
                       ("XDG_SESSION_TYPE" . "x11")
                       ("XDG_SESSION_DESKOP" . "stumpwm")
                       ("XDG_CURRENT_DESKTOP" . "stumpwm")
                       ;; ("XDG_DOWNLOAD_DIR" . "/home/logoraz/Downloads")
                       ("GUILE_WARN_DEPRECATED" . "detailed")))
     (service home-bash-service-type
              (home-bash-configuration
               (guix-defaults? #f)
               (aliases '(("grep" . "grep --color=auto")
                          ("ls"   . "ls -p --color=auto")
                          ("ll"   . "ls -l")
                          ("la"   . "ls -la")
                          ("gsr"  . "sudo guix system reconfigure")))
               (bashrc
                (list (local-file "config/guix/dot-bashrc.sh"
                                  #:recursive? #t)))
               (bash-profile
                (list (local-file "config/guix/dot-bash_profile.sh"
                                  #:recursive? #t)))))))))


;;; System Configuration Definitions
(define locutus-keyboard-layout
  (keyboard-layout "us"))

(define logoraz-user-account
  (list (user-account
         (name "logoraz")
         (comment "Erik P. Almaraz")
         (group "users")
         (home-directory "/home/logoraz")
         (supplementary-groups '("wheel" "netdev" "audio" "video" "lp")))))

(define locutus-file-system
  (list (file-system
         (mount-point  "/boot/efi")
         (device (uuid "F8E9-9C22" 'fat32))
         (type "vfat"))
        (file-system
         (mount-point "/")
         (device (uuid "c0ffc6f4-dab7-4efc-8cdd-3e9d727b91ab" 'ext4))
         (type "ext4"))))

;;; System Package Transformations
;; Apply in package list (latest-sbcl sbcl)
(define latest-sbcl
  (options->transformation
   '((with-latest . "sbcl"))))

;;; Define Core System Wide Packages & Services

;; Ensure the fundamental Common Lisp implementations are available on my system.
;; TODO: Later to install fundamental cl libraries that every system should have.
(define cl-packages
  (list ccl
        ;;clasp-cl
        cl-iterate
        cl-bordeaux-threads
        cl-lparallel))

(define stumpwm-packages
  (list sbcl                       ;;|--> gnu packages lisp
        sbcl-zippy                 ;;|--> gnu packages lisp-xyz
        sbcl-parse-float
        sbcl-local-time
        sbcl-cl-ppcre
        sbcl-zpng
        sbcl-salza2
        sbcl-clx
        sbcl-zpb-ttf
        sbcl-cl-vectors
        sbcl-cl-store
        sbcl-trivial-features
        sbcl-global-vars
        sbcl-trivial-garbage
        sbcl-bordeaux-threads
        sbcl-cl-fad
        sbcl-clx-truetype
        stumpwm+slynk                    ;;|--> gnu packages wm
        sbcl-stumpwm-ttf-fonts     ;;:stumpwm-contrib/util
        sbcl-stumpwm-kbd-layouts
        sbcl-stumpwm-swm-gaps
        sbcl-stumpwm-globalwindows
        sbcl-stumpwm-cpu           ;;:stumpwm-contrib/modeline
        sbcl-stumpwm-mem
        sbcl-stumpwm-wifi
        sbcl-stumpwm-battery-portable))

(define x11-util-packages
  (list font-hack           ;;|--> gnu packages fonts
        font-jetbrains-mono
        xterm               ;;|--> gnu packages xorg
        transset
        xhost
        xset
        xsetroot
        xinput
        xrdb
        xrandr
        xclip               ;;|--> gnu packages xdisorg
        xsel
        xss-lock
        xdg-utils           ;;|--> gnu packages freedesktop
        blueman             ;;|--> gnu package networking
        bluez))

;; System Services
;; Use Package substitutes instead of compiling everything & specify channels
;; https://guix.gnu.org/manual/en/html_node/Getting-Substitutes-from-Other-Servers.html
(define (substitutes->services config channels)
  (guix-configuration
   (inherit config)
   (channels channels)
   (guix (guix-for-channels channels))
   (substitute-urls
    (cons* "https://substitutes.nonguix.org"
           "https://ci.guix.gnu.org"
           %default-substitute-urls))
   (authorized-keys
    (cons* (origin
            (method url-fetch)
            (uri "https://substitutes.nonguix.org/signing-key.pub")
            (file-name "nonguix.pub")
            (sha256
             (base32
              "0j66nq1bxvbxf5n8q2py14sjbkn57my0mjwq7k1qm9ddghca7177")))
           %default-authorized-guix-keys))))

(define guix-system-services
  (cons*
   (set-xorg-configuration
    (xorg-configuration
     (keyboard-layout locutus-keyboard-layout)))
   (service screen-locker-service-type
            (screen-locker-configuration
             (name "slock")
             (program (file-append slock "/bin/slock"))))
   ;; See: https://guix.gnu.org/manual/en/html_node/Desktop-Services.html
   (service bluetooth-service-type
            (bluetooth-configuration
             (auto-enable? #f)))
   (service cups-service-type
            (cups-configuration
             (web-interface? #t)
             (default-paper-size "Letter")
             (extensions (list cups-filters hplip-minimal))))
   ;; ssh user@host -p 2222
   (service openssh-service-type
            (openssh-configuration
             (openssh openssh)
             (port-number 2222)))
   ;; Set up my home configuration
   (service guix-home-service-type
            `(("logoraz" ,logoraz-home)))
   (modify-services %desktop-services
                    (guix-service-type
                     config =>
                     (substitutes->services config logoraz-channels)))))

;;;; Define Operating system
(define guix-os
  (operating-system
   (kernel linux)
   (initrd microcode-initrd)
   ;; Fixes Xorg Lag - https://gitlab.com/nonguix/nonguix/-/issues/212
   (kernel-arguments (cons "i915.enable_psr=0" %default-kernel-arguments))
   (firmware (list linux-firmware))
   (locale "en_US.utf8")
   (timezone "America/Los_Angeles")
   (keyboard-layout locutus-keyboard-layout)
   (host-name "locutus")

   ;; List of user accounts ('root' is implicit).
   (users (append
           logoraz-user-account
           %base-user-accounts))

   (bootloader (bootloader-configuration
                (bootloader grub-efi-bootloader)
                (targets (list "/boot/efi"))
                (keyboard-layout keyboard-layout)))

   (swap-devices (list (swap-space
                        (target
                         (uuid
			  "b547f9c1-9a69-4c63-9c55-edc2736bf504")))))

   ;; Use 'blkid' to find unique file system identifiers ("UUIDs").
   (file-systems (append
                  locutus-file-system
		  %base-file-systems))

   ;; Use 'guix search KEYWORD' to search for packages.
   (packages (append
              cl-packages
              stumpwm-packages
              x11-util-packages
              %base-packages))

   (services guix-system-services)))

;;; Instantiate Guix-OS
guix-os
