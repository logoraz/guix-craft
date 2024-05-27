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
         (device (uuid
                  "0E5C-C5DD"
                  'fat32))
         (type "vfat"))
        (file-system
         (mount-point "/")
         (device (uuid
                  "4d080674-56cf-4eb0-a6d4-a9e6827aa957"
                  'ext4))
         (type "ext4"))
        (file-system
         (mount-point "/home")
         (device (uuid
                  "58e6f2e5-89af-4e8c-abfd-9dc7714b2513"
                  'ext4))
         (type "ext4"))))

;; Define Core System Wide Packages & Services
(define %stumpwm-packages
  (list
   "sbcl"
   "stumpwm-with-slynk"
   "stumpwm:lib"))

(define %system-services
  (cons*
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
   %desktop-services))

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
                        "1d78e3c5-3776-4774-bfad-dc0850a21f1c")))))

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
