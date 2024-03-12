(use-modules (gnu)
             (gnu packages) ;; to use specification->package
             (gnu services)
             (nongnu packages linux))

(use-service-modules desktop ssh xorg)

(operating-system
 (kernel linux)
 (firmware (list linux-firmware))
 (locale "en_US.utf8")
 (timezone "America/Los_Angeles")
 (keyboard-layout (keyboard-layout "us"))
 (host-name "thinkpad")

 (users (cons (user-account
               (name "erik")
               (comment "Erik Almaraz")
               (group "users")
               (home-directory "/home/erik")
               (supplementary-groups '("wheel" "netdev" "audio" "video")))
              %base-user-accounts))

 (packages (append
            (map specification->package
                 (list
                  ;; Core programs
                  "emacs" "git" "guile" "ccl"
                  ;; window manager
                  "sbcl" "stumpwm"
                  ;; for HTTPS access
                  "nss-certs"))
            %base-packages))

 (services (append
            (list
             (service openssh-service-type)
             (set-xorg-configuration
              (xorg-configuration
               (keyboard-layout keyboard-layout))))
            %desktop-services))

 (bootloader (bootloader-configuration
              (bootloader grub-efi-bootloader)
              (targets (list "/boot/efi"))
              (keyboard-layout keyboard-layout)))

 (swap-devices (list
                (swap-space
                 (target (uuid
                          "250048df-e256-406e-8d7d-f7005496ab94")))))

 (file-systems (cons*
                (file-system
                 (mount-point "/boot/efi")
                 (device (uuid
                          "0E5C-C5DD"
                          'fat32))
                 (type "vfat"))
                (file-system
                 (mount-point "/")
                 (device (uuid
                          "56c2d13f-1179-4786-a138-3c5835e5739c"
                          'ext4))
                 (type "ext4"))
                %base-file-systems)))
