;;;; System-wide Configuration
;;
(use-modules (gnu)
	     (gnu packages)
             (gnu packages cups)
             (gnu services)
             (gnu services cups)
             (gnu services desktop)
             (gnu services ssh)
             (gnu services xorg)
	     (nongnu packages linux))

(operating-system
 (kernel linux)
 (firmware (list linux-firmware))
 (locale "en_US.utf8")
 (timezone "America/Los_Angeles")
 (keyboard-layout (keyboard-layout "us"))
 (host-name "logos")
 
 ;; List of user accounts ('root' is implicit).
 (users (append
         (list (user-account
                (name "raiz")
                (comment "Erik P. Almaraz")
                (group "users")
                (home-directory "/home/raiz")
                (supplementary-groups '("wheel" "netdev" "audio" "video"))))
         %base-user-accounts))

 (bootloader (bootloader-configuration
              (bootloader grub-efi-bootloader)
              (targets (list "/boot/efi"))
              (keyboard-layout keyboard-layout)))

 (swap-devices (list (swap-space
                      (target (uuid
                               "1d78e3c5-3776-4774-bfad-dc0850a21f1c")))))

  ;; Use 'blkid' to find unique file system identifiers ("UUIDs").
 (file-systems (append
                (list (file-system
                       (mount-point "/boot/efi")
                       (device (uuid "0E5C-C5DD"
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
                       (type "ext4")))
		%base-file-systems))

 ;; Use 'guix search KEYWORD' to search for packages.
 (packages (append
            (map specification->package
                 (list
                  ;; Add other system-wide packages here...
                  "nss-certs"))
            %base-packages))

 ;; Use 'guix system search KEYWORD' to search for system services.
 (services (append
            (list (service cups-service-type
                           (cups-configuration
                            (web-interface? #t)
                            (default-paper-size "Letter")
                            (extensions (list cups-filters hplip-minimal))))
                  ;; TODO configure OpenSSH - `openssh-configuration'
                  (service openssh-service-type)
                  ;; See: https://guix.gnu.org/manual/en/html_node/X-Window.html
                  (set-xorg-configuration
                   (xorg-configuration (keyboard-layout keyboard-layout))))
            %desktop-services)))
