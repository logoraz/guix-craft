;; This is an operating system configuration generated
;; by the graphical installer.
;;
;; Once installation is complete, you can learn and modify
;; this file to tweak the system configuration, and pass it
;; to the 'guix system reconfigure' command to effect your
;; changes.


;; Indicate which modules to import to access the variables
;; used in this configuration.
(use-modules (gnu)
	     (nongnu packages linux))

(use-service-modules cups desktop networking ssh xorg)

(operating-system
 (kernel linux)
 (firmware (list linux-firmware))
 (locale "en_US.utf8")
 (timezone "America/Los_Angeles")
 (keyboard-layout (keyboard-layout "us"))
 (host-name "locutus")
 
 ;; The list of user accounts ('root' is implicit).
 (users (cons* (user-account
                (name "logoraz")
                (comment "Erik P. Almaraz")
                (group "users")
                (home-directory "/home/logoraz")
                (supplementary-groups '("wheel" "netdev" "audio" "video")))
               %base-user-accounts))

 ;; Packages installed system-wide.  Users can also install packages
 ;; under their own account: use 'guix search KEYWORD' to search
 ;; for packages and 'guix install PACKAGE' to install a package.
 (packages (append (list (specification->package "nss-certs"))
                   %base-packages))
 
 ;; Below is the list of system services.  To search for available
 ;; services, run 'guix system search KEYWORD' in a terminal.
 (services
  (append (list (service gnome-desktop-service-type)
		
                ;; To configure OpenSSH, pass an 'openssh-configuration'
                ;; record as a second argument to 'service' below.
                (service openssh-service-type)
                (set-xorg-configuration
                 (xorg-configuration (keyboard-layout keyboard-layout))))
	  
          ;; This is the default list of services we
          ;; are appending to.
          %desktop-services))
 (bootloader (bootloader-configuration
              (bootloader grub-efi-bootloader)
              (targets (list "/boot/efi"))
              (keyboard-layout keyboard-layout)))
 (swap-devices (list (swap-space
                      (target (uuid
                               "641df639-73e4-4c83-8620-c49fba958e87")))))
 
 ;; The list of file systems that get "mounted".  The unique
 ;; file system identifiers there ("UUIDs") can be obtained
 ;; by running 'blkid' in a terminal.
 (file-systems (cons* (file-system
                       (mount-point "/boot/efi")
                       (device (uuid "81F3-AB0A"
                                     'fat32))
                       (type "vfat"))
                      (file-system
                       (mount-point "/")
                       (device (uuid
                                "c9a9da72-8f59-4230-b57d-2b4aec4093d5"
                                'ext4))
                       (type "ext4")) %base-file-systems)))
