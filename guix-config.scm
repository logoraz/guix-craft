;;;; Guix SYSTEM Configuration

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
	     (guix download)
             (guix gexp)
             (guix transformations)
	     (nongnu packages linux)
             (nongnu system linux-initrd))

(use-package-modules lisp lisp-xyz wm xorg xdisorg linux fonts
                     cups suckless networking
                     fonts web-browsers gnuzilla password-utils gnupg mail
                     gstreamer video compton image-viewers linux music
                     gnucash gimp inkscape graphics compression version-control
                     guile guile-xyz emacs emacs-xyz)

(use-service-modules guix cups ssh desktop xorg)

(define %keyboard-layout
  (keyboard-layout "us"))

(define %logoraz-user-account
  (list (user-account
         (name "logoraz")
         (comment "Erik P. Almaraz")
         (group "users")
         (home-directory "/home/logoraz")
         (supplementary-groups '("wheel" "netdev" "audio" "video" "lp")))))

(define %locutus-file-system
  (list (file-system
         (mount-point  "/boot/efi")
         (device (uuid "F8E9-9C22" 'fat32))
         (type "vfat"))
        (file-system
         (mount-point "/")
         (device (uuid "c0ffc6f4-dab7-4efc-8cdd-3e9d727b91ab" 'ext4))
         (type "ext4"))))

;; Define Core System Wide Packages & Services
(define %stumpwm-packages
  (list sbcl                       ;;|--> gnu packages lisp
        sbcl-slynk                 ;;|--> gnu packages lisp-xyz
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
        stumpwm+slynk              ;;|--> gnu packages wm
        sbcl-stumpwm-ttf-fonts     ;;:stumpwm-contrib/util
        sbcl-stumpwm-kbd-layouts
        sbcl-stumpwm-swm-gaps
        sbcl-stumpwm-globalwindows
        sbcl-stumpwm-cpu           ;;:stumpwm-contrib/modeline
        sbcl-stumpwm-mem
        sbcl-stumpwm-wifi
        sbcl-stumpwm-battery-portable))

(define %x11-util-packages
  (list font-hack    ;;|--> gnu packages fonts
        font-jetbrains-mono
        xterm        ;;|--> gnu packages xorg
        transset
        xhost
        xset
        xsetroot
        xinput
        xrdb
        xrandr
        xclip        ;;|--> gnu packages xdisorg
        xsel
        xss-lock
        blueman      ;;|--> gnu package networking
        bluez))

(define %logoraz-packages
  (list font-fira-code  ;;|--> gnu packages fonts
        font-iosevka-aile
        font-google-noto
        font-google-noto-emoji
        font-google-noto-sans-cjk
        nyxt            ;;|--> gnu packages web-browsers :www-mail
        icecat          ;;|--> gnu packages gnuzilla
        keepassxc       ;;|--> gnu packages password-utils
        gnupg           ;;|--> gnu packages gnupg
        isync           ;;|--> gnu packages mail
        msmtp
        mu
        gstreamer       ;;|--> gnu packages gstreamer
        gst-plugins-good
        gst-plugins-bad
        gst-libav
        mpv             ;;|--> gnu packages video :apps
        vlc
        picom           ;;|--> gnu packages compton
        feh             ;;|--> gnu packages image-viewers
        pipewire        ;;|--> gnu packages linux
        wireplumber
        lm-sensors
        brightnessctl
        playerctl       ;;|--> gnu packages music
        gnucash         ;;|--> gnu packages gnucash
        gimp            ;;|--> gnu packages gimp
        inkscape        ;;|--> gnu packages inkscape
        blender         ;;|--> gnu packages graphics
        zip             ;;|--> gnu packages compression :utilities
        unzip
        git))           ;;|--> gnu packages version-control

(define %emacs-packages
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

(define logoraz-home
  (home-environment
   ;; Below is the list of packages that will show up in your
   ;; Home profile, under ~/.guix-home/profile.
   (packages (append
              %logoraz-packages
              %emacs-packages))

   ;; Below is the list of Home services.  To search for available
   ;; services, run 'guix home search KEYWORD' in a terminal.
   ;; TODO: Implement the following
   ;; 1. (service home-xdg-configuration-files-service-type ...)
   ;; 2. (service home-files-service-type ...)
   ;; 3. (service home-dotfiles-service-type ..)
   (services
    (list
     (service home-pipewire-service-type)
     (service home-dbus-service-type) ;; for bluetooth --> system
     (simple-service 'env-vars home-environment-variables-service-type
                     '(("EDITOR" . "emacs")
                       ("BROWSER" . "nyxt")
                       ("XDG_SESSION_TYPE" . "x11")
                       ("XDG_SESSION_DESKOP" . "stumpwm")
                       ("XDG_CURRENT_DESKTOP" . "stumpwm")
                       ("XDG_DOWNLOAD_DIR" . "/home/logoraz/Downloads/")))
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
                          ("gud"  . "guix system delete-generations")
                          ("ghd"  . "guix home delete-generations")))
               (bashrc
                (list (local-file "./dot-bashrc.sh"
                                  #:recursive? #t)))
               (bash-profile
                (list (local-file "./dot-bash_profile.sh"
                                  #:recursive? #t)))))))))

;; Use Package substitutes instead of compiling everything
;; Borrowed from iambumblehead
(define (substitutes->services config)
  (guix-configuration
   (inherit config)
   (substitute-urls
    (cons* "https://substitutes.nonguix.org"
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

(define %system-services
  (cons*
   (set-xorg-configuration
    (xorg-configuration
     (keyboard-layout %keyboard-layout)))
   (service screen-locker-service-type
            (screen-locker-configuration
             (name "slock")
             (program (file-append slock "/bin/slock"))))
   ;; Don't believe there exists a home service type for bluetooth
   (service bluetooth-service-type
            (bluetooth-configuration
             (auto-enable? #t)))
   (service cups-service-type
            (cups-configuration
             (web-interface? #t)
             (default-paper-size "Letter")
             (extensions (list cups-filters hplip-minimal))))
   (service openssh-service-type)
   ;; Set up my home configuration
   (service guix-home-service-type
    `(("logoraz" ,logoraz-home)))
   (modify-services %desktop-services
                    (guix-service-type
                     config =>
                     (substitutes->services config)))))

;; Define Operating system
(operating-system
 (kernel linux)
 (initrd microcode-initrd)
 ;; Fixes Xorg Lag - https://gitlab.com/nonguix/nonguix/-/issues/212
 (kernel-arguments (cons "i915.enable_psr=0" %default-kernel-arguments))
 (firmware (list linux-firmware))
 (locale "en_US.utf8")
 (timezone "America/Los_Angeles")
 (keyboard-layout %keyboard-layout)
 (host-name "locutus")
 
 ;; List of user accounts ('root' is implicit).
 (users (append
         %logoraz-user-account
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
                %locutus-file-system
		%base-file-systems))

 ;; Use 'guix search KEYWORD' to search for packages.
 (packages (append
            %stumpwm-packages
            %x11-util-packages
            %base-packages))

 (services %system-services))
