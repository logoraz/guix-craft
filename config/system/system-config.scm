(define-module (config system system-config)
  #:use-module (gnu)
  #:use-module (gnu packages)
  #:use-module (gnu services)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix transformations)
  #:use-module (guix ci)
  #:use-module (nongnu packages linux)
  #:use-module (nongnu system linux-initrd))

(use-system-modules keyboard)

(use-package-modules lisp lisp-xyz wm xorg xdisorg linux fonts freedesktop
                     ssh cups suckless networking package-management)

(use-service-modules cups ssh desktop xorg)


;; Packages
(define latest-sbcl
  (options->transformation
   '((with-latest . "sbcl"))))

(define stumpwm-packages
  (list (latest-sbcl sbcl)         ;;|--> gnu packages lisp
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
        stumpwm+slynk              ;;|--> gnu packages wm
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


(define locutus-keyboard-layout
  (keyboard-layout "us"))

;; System Services
;; Use Package substitutes instead of compiling everything & specify channels
;; https://guix.gnu.org/manual/en/html_node/Getting-Substitutes-from-Other-Servers.html
(define (substitutes->services config)
  (guix-configuration
   (inherit config)
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
   (modify-services %desktop-services
                    (guix-service-type
                     config =>
                     (substitutes->services config)))))


;;;; Define Operating system
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
              stumpwm-packages
              x11-util-packages
              %base-packages))

   (services guix-system-services)))

;;; Instantiate Guix-OS
guix-os
