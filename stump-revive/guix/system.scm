;;;; Guix SYSTEM Configuration

(use-modules (gnu)
	     (gnu packages)
             (gnu packages cups)       ; -> needed for cups service
             (gnu packages suckless)   ; -> need for lock service
             (gnu services)
             (gnu services cups)
             (gnu services ssh)
             (gnu services xorg)
             (gnu services desktop)
	     (guix packages)
	     (guix download)
	     (nongnu packages linux))


(define %keyboard-layout
  (keyboard-layout "us"))

(define %logoraz-user-account
  (list (user-account
         (name "logoraz")
         (comment "Erik P. Almaraz")
         (group "users")
         (home-directory "/home/logoraz")
         (supplementary-groups '("wheel" "netdev" "audio" "video")))))

(define %locutus-file-system
  (list (file-system
         (mount-point  "/boot/efi")
         (device (uuid "F8E9-9C22" 'fat32))
         (type "vfat"))
        (file-system
         (mount-point "/")
         (device (uuid "5e240887-d76c-4833-a2e7-db50909e6ab2" 'ext4))
         (type "ext4"))))

;; Define Core System Wide Packages & Services
(define %stumpwm-packages
  (list
   "sbcl"
   "stumpwm-with-slynk"
   "stumpwm:lib"))

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
   ;; Not really sure if `xorg-configruation' is truly needed.
   (set-xorg-configuration
    (xorg-configuration
     (keyboard-layout %keyboard-layout)))
   (service screen-locker-service-type
            (screen-locker-configuration
             (name "slock")
             (program (file-append slock "/bin/slock"))))
   (service cups-service-type
            (cups-configuration
             (web-interface? #t)
             (default-paper-size "Letter")
             (extensions (list cups-filters hplip-minimal))))
   (service openssh-service-type)
   ;; Set up my home configuration
   ;; (guix-home-service-type
   ;;  `(("logoraz" ,home)))
   ;; Supposed to make package installation faster, but has EXTREMELY slow downloads
   (modify-services %desktop-services
                    (guix-service-type
                     config =>
                     (substitutes->services config)))))

;; Define Operating system
(operating-system
 (kernel linux)
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
			"75f1a5fb-3543-4129-915a-64ebf91ca4ff")))))

 ;; Use 'blkid' to find unique file system identifiers ("UUIDs").
 (file-systems (append
                %locutus-file-system
		%base-file-systems))

 ;; Use 'guix search KEYWORD' to search for packages.
 (packages (append
            (specifications->packages
             %stumpwm-packages)
            %base-packages))

 (services %system-services))
