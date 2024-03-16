;; This "home-environment" file can be passed to 'guix home reconfigure'
;; to reproduce the content of your profile.  This is "symbolic": it only
;; specifies package names.  To reproduce the exact same profile, you also
;; need to capture the channels being used, as returned by "guix describe".
;; See the "Replicating Guix" section in the manual.

;; guix home: 'guix-conf/' populated with all the Home configuration files
;; hint: Run `guix home reconfigure guix-conf//home-configuration.scm' to
;; effectively deploy the home environment described by these files.

(use-modules (gnu home)
             (gnu packages)
             (gnu services)
             (guix gexp)
             (gnu home services shells))

(define %packages
  (list
   ;; IDE / Dev Tools
   "ccl"
   "make"
   "guile"
   "git"
   "git:send-email"
   "curl"
   "gdb"
   "guile-ares-rs"
   "guile-hoot"

   ;;Terminals
   ;;
   ;; Fonts
   "font-fira-code"
   "font-iosevka-aile"
   "font-dejavu"
   "font-google-material-design-icons"
   ;;; www/mail
   "nyxt"
   "gnupg"
   "keepassxc"
   "gstreamer"
   "gst-plugins-good"
   "gst-plugins-bad"
   "gst-libav"
   "mu"
   "msmtp"
   "isync"
   "icecat"
   ;;; Apps
   "gnucash"
   "vlc"
   "gimp"
   "inkscape"
   ;;; Mail
   ;;; Documents
   "texlive-scheme-basic"
   "texlive-collection-latexrecommended"
   "texlive-collection-fontsrecommended"
   ;;; Windows System Utils
   "lm-sensors"
   "xrdb"
   "xrandr"
   "libnotify"
   "picom"
   "alsa-utils"
   "xss-lock"
   "slock"
   "playerctl"
   "brightnessctl"
   "scrot"
   "feh"
   "xcursor-themes"
   "pinentry"
   ;; Utils
   "binutils"
   "xhost"
   "xset"
   "pavucontrol"
   "nm-tray"
   "blueman"
   "pasystray"
))

(define %stumpwm-packages
  (list
   ;; Window Manager
   "sbcl"
   "stumpwm"
   "stumpwm:lib"
   ;; WM Support Modules
   "sbcl-stumpwm-ttf-fonts"
   "sbcl-stumpwm-kbd-layouts"
   "sbcl-stumpwm-swm-gaps"
   "sbcl-stumpwm-cpu"
   "sbcl-stumpwm-disk"
   "sbcl-stumpwm-mem"
   "sbcl-stumpwm-net"
   "sbcl-stumpwm-wifi"
   "sbcl-stumpwm-battery-portable"
   "sbcl-stumpwm-hostname"
   "sbcl-stumpwm-globalwindows"
   "sbcl-stumpwm-winner-mode"
   "sbcl-stumpwm-notify"
   "sbcl-stumpwm-screenshot"
   "sbcl-stumpwm-pamixer"))


  (define %emacs-packages
    (list
     "emacs"
     "emacs-no-littering"
     "emacs-undo-tree"
     "emacs-ws-butler"
     "emacs-ligature"
     "emacs-nord-theme"
     "emacs-magit"
     "emacs-mct"
     "emacs-marginalia"
     "emacs-orderless"
     "emacs-corfu"
     "emacs-paredit"
     "emacs-vterm"
     "emacs-beframe"
     "emacs-sly"
     "emacs-guix"
     "emacs-arei"
     "emacs-denote"
     "emacs-nyxt"
     "emacs-stumpwm-mode"
     "emacs-org-superstar"
     "emacs-org-appear"
     "emacs-mbsync"
     "emacs-bongo"))

(home-environment
 ;; Below is the list of packages that will show up in your
 ;; Home profile, under ~/.guix-home/profile.
 (packages (append (map specification->package+output
                        (append
                         %packages
			 %stumpwm-packages
                         %emacs-packages))))
 
 ;; Below is the list of Home services.  To search for available
 ;; services, run 'guix home search KEYWORD' in a terminal.
 (services
  (list
   (service home-bash-service-type
            (home-bash-configuration
	     (guix-defaults? #f)
             (aliases '(("grep" . "grep --color=auto")
                        ("ll"   . "ls -l")
                        ("la"   . "ls -la")
                        ("ls"   . "ls -p --color=auto")
                        ("ghr"  . "guix home reconfigure")
                        ("gsr"  . "sudo guix system reconfigure")
                        ("gup"  . "guix pull && guix upgrade")
                        ("gud"  . "guix system delete-generations")
                        ("ghd"  . "guix home delete-generations")))
             (bashrc
              (list (local-file "./config/dot-bashrc.sh"
                                #:recursive? #t)))
             (bash-profile
              (list (local-file "./config/dot-bash_profile.sh"
                                #:recursive? #t))))))))
