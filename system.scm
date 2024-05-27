;;;; Guix SYSTEM Configuration

(use-modules (gnu)
	     (gnu packages)
	     (gnu services)
             (gnu services xorg)
             (gnu services ssh)
             (gnu services cups)
             (gnu services desktop)
             (gnu services networking)	     
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
         (mount-point "/boot/efi")
         (device (uuid "81F3-AB0A" 'fat32))
         (type "vfat"))
        (file-system
         (mount-point "/")
         (device (uuid "c9a9da72-8f59-4230-b57d-2b4aec4093d5" 'ext4))
         (type "ext4"))))

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

;; Define Core System Wide Packages & Services
(define %system-services
  (cons*
   (service gnome-desktop-service-type)
   (service openssh-service-type)
   ;; Set up my home configuration
   ;; (guix-home-service-type
   ;;  `(("logoraz" ,home)))
   (modify-services %desktop-services
                    (guix-service-type
                     config =>
                     (substitutes->services config)))))

(define %system-packages
  (list "tbd"))

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

 (swap-devices (list
                (swap-space
                 (target
		  (uuid
                   "641df639-73e4-4c83-8620-c49fba958e87")))))
 
 ;; Use 'blkid' to find unique file system identifiers ("UUIDs").
 (file-systems (append
                %locutus-file-system
		%base-file-systems))

 ;; Use 'guix search KEYWORD' to search for packages.
 (packages %base-packages)
 ;; WIP
 ;; (packages (append
 ;;            (specifications->packages
 ;;             %system-packages)
 ;;            %base-packages))

 (services %system-services))
