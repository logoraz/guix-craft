;;;; GUIX HOME Configuration
;;

(use-modules (gnu home)
             (gnu packages)
             (gnu services)
             (guix gexp)
             (gnu home services desktop)
             (gnu home services sound)
             (gnu home services shells))

(define %logoraz-packages
  (list
   ;; Dev Tools / IDE
   "guile-next"
   "guile-ares-rs"
   "guile-hoot"
   "ccl"
   "gdb"
   "make"
   "binutils"
   "git"
   "git:send-email"
   "curl"
   ;; Fonts
   "font-fira-code"
   "font-iosevka-aile"
   "font-dejavu"
   "font-google-material-design-icons"
   ;; WWW/Mail
   "nyxt"
   "icecat"
   "gnupg"
   "pinentry"
   "keepassxc"
   "isync"
   "msmtp"
   "mu"
   "gstreamer"
   "gst-plugins-good"
   "gst-plugins-bad"
   "gst-libav"
   ;; Apps
   "mpv"
   "vlc"
   "gnucash"
   "gimp"
   "inkscape"
   ;; Documents
   "texlive-scheme-basic"
   "texlive-collection-latexrecommended"
   "texlive-collection-fontsrecommended"
   ;; Windows System Utils
   ;; see https://gitlab.freedesktop.org/xorg/app/transset
   ;; https://unix.stackexchange.com/questions/127624/make-xterm-transparent
   "transset"
   "picom"
   "feh"
   "scrot"
   "pipewire"
   "wireplumber"
   "playerctl"
   "lm-sensors"
   "libnotify"
   "brightnessctl"
   "blueman"))

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
   "sbcl-stumpwm-globalwindows"
   "sbcl-stumpwm-notify"
   "sbcl-stumpwm-winner-mode"
   "sbcl-stumpwm-screenshot"
   "sbcl-parse-float" ;; needed for stumpwm-wpctl -> now audio-wpctl
   ;; mode-line support
   "sbcl-stumpwm-cpu"
   "sbcl-stumpwm-mem"
   "sbcl-stumpwm-wifi"
   "sbcl-stumpwm-battery-portable"))

  (define %emacs-packages
    (list
     "emacs"
     "emacs-nord-theme"
     "emacs-ligature"
     "emacs-no-littering"
     "emacs-ws-butler"
     "emacs-undo-tree"
     "emacs-paredit"
     "emacs-mct"
     "emacs-orderless"
     "emacs-corfu"
     "emacs-marginalia"
     "emacs-beframe"
     "emacs-denote"
     "emacs-magit"
     "emacs-vterm"
     "emacs-guix"
     "emacs-arei" ;; This is no longer working...
     "emacs-sly"
     "emacs-nyxt"
     "emacs-stumpwm-mode"
     "emacs-mbsync"
     "emacs-org-superstar"
     "emacs-org-appear"
     "emacs-bongo"))

(home-environment
 ;; Below is the list of packages that will show up in your
 ;; Home profile, under ~/.guix-home/profile.
 (packages (specifications->packages
            (append
                %logoraz-packages
		%stumpwm-packages
                %emacs-packages)))
 
 ;; Below is the list of Home services.  To search for available
 ;; services, run 'guix home search KEYWORD' in a terminal.
 (services
  (list
   ;; (service home-x11-service-type)
   (service home-dbus-service-type)
   (service home-pipewire-service-type)
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
              (list (local-file "./config/dot-bashrc.sh"
                                #:recursive? #t)))
             (bash-profile
              (list (local-file "./config/dot-bash_profile.sh"
                                #:recursive? #t))))))))
