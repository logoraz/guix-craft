;;;; System-wide Configuration
;; GUIX System Configuration
;;

(use-modules (gnu)
	     (gnu packages)
             (gnu packages cups)     ;;needed for cups service
             (gnu packages suckless) ;;need for lock service
             (gnu services)
             (gnu services cups)
             (gnu services ssh)
             (gnu services xorg)
             (gnu services desktop)
             (nongnu system linux-initrd)
	     (nongnu packages linux))

(define %xorg-packages
  (list
   "xhost"
   "xset"
   "xsetroot"
   "xinput"
   "xrdb"
   "xrandr"
   "xterm"
   "xclip"
   "xsel"
   "xss-lock"
   "slock"))

(define %sys-packages
  (list
   "nss-certs"))

(operating-system
 (kernel linux)
 ;; Fixes Xorg Lag - https://gitlab.com/nonguix/nonguix/-/issues/212
 (kernel-arguments (cons "i915.enable_psr=0" %default-kernel-arguments))
 (initrd microcode-initrd)
 (initrd-modules (cons "i915" %base-initrd-modules))
 (firmware (list linux-firmware))
 (locale "en_US.utf8")
 (timezone "America/Los_Angeles")
 (keyboard-layout (keyboard-layout "us"))
 (host-name "locutus")
 
 ;; List of user accounts ('root' is implicit).
 ;; trial chaing home-directory on next system reconfigure to see what it does...
 ;; previous username was raiz, /home/raiz
 (users (append
         (list (user-account
                (name "logoraz")
                (comment "Erik P. Almaraz")
                (group "users")
                (home-directory "/home/logoraz")
                (supplementary-groups '("wheel" "netdev" "audio" "video"))))
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
                       (type "ext4")))
		%base-file-systems))

 ;; Use 'guix search KEYWORD' to search for packages.
 (packages (append
            (map specification->package
                 (append
                  %xorg-packages
                  %sys-packages))
            %base-packages))

 ;; Use 'guix system search KEYWORD' to search for system services.
 (services (append
            (list (service cups-service-type
                           (cups-configuration
                            (web-interface? #t)
                            (default-paper-size "Letter")
                            (extensions (list cups-filters hplip-minimal))))
                  ;;TODO configure OpenSSH - `openssh-configuration'
                  (service openssh-service-type)
                  ;; See: guix-cookbook 3.7.2.1 Xorg
                  ;; FIXME: Screen locking not working as prescribed by the manual.
                  (service screen-locker-service-type
                           (screen-locker-configuration
                            (name "slock")
                            (program (file-append slock "/bin/slock"))))
                  ;;TODO: Is there a way to add a nice font to the default
                  ;;      X fonts?
                  ;; See: https://guix.gnu.org/manual/en/html_node/X-Window.html
                  (set-xorg-configuration
                   (xorg-configuration (keyboard-layout keyboard-layout))))
            %desktop-services)))
