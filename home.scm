;;;; Guix HOME Configuration

(use-modules (gnu home)
             (gnu home services)
             (gnu home services desktop)
             (gnu home services sound)
             (gnu home services shells)
             (gnu packages)
             (gnu services)
             (guix gexp))

(define %logoraz-packages
  (list
   ;; Fonts
   "font-fira-code"
   "font-nerd-fonts-fira-code"    ; --> benoitj channel
   "font-iosevka-aile"
   "font-dejavu"
   "font-google-noto"
   "font-google-noto-emoji"
   "font-google-material-design-icons"
   "font-google-noto-sans-cjk"
   ;; WWW/Mail
   "nyxt"
   "icecat"
   "gnupg"
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
   "blender"
   ;; Documents/Files
   ;; "texlive-scheme-basic"
   ;; "texlive-collection-latexrecommended"
   ;; "texlive-collection-fontsrecommended"
   "zip"
   "unzip"))

(define %dev-packages  ;-> perhpas move to a manifest
  (list
   ;; Guile Dev Tools
   "guile-next"        ; needed for ares/arei
   "guile-ares-rs"     ; for mREPL Guile Scheme Emacs IDE
   "guile-hoot"        ; explore Web/WASM
   ;; Common Lisp Dev Tools
   "ccl"
   "ecl"
   ;; Base Dev Tools
   ;; "elfutils"      ; need to determine what this does...
   ;; "libtree"       ; not sure if I need
   "gcc-toolchain"
   "make"
   "binutils"
   "curl"
   "git"
   "git:send-email"))

(define %xorg-util-packages
  (list
   ;; Xorg Window System Utils
   ;; see -> https://gitlab.freedesktop.org/xorg/app/transset
   ;; see -> https://unix.stackexchange.com/questions/127624/make-xterm-transparent
   "xhost"                   ;
   "xset"                    ;
   "xsetroot"                ;
   "xinput"                  ;
   "xrdb"                    ; Set Xresource files
   "xrandr"                  ; Screen rendering
   "xclip"                   ; Clipboard
   "xsel"                    ;
   "xss-lock"                ; Screen locking
   "xterm"                   ; XORG Terminal
   "transset"                ; XORG window transparency
   "picom"                   ; Compositor
   "feh"                     ; Desktop background
   "pipewire"                ; Audio System
   "wireplumber"             ; Audio System router/controls
   "playerctl"               ; Audio Player
   "lm-sensors"              ; CLI system monitor
   "libnotify"               ; Notifications
   "brightnessctl"           ; Brightness Controls
   "blueman"                 ; Bluetooh System/Controls (-> need to configure)
   "bluez"))                 ; Bluetooh API

(define %stumpwm-packages
  (list
   ;; StumpWM Support Modules
   "sbcl-slynk"                  ;
   "sbcl-parse-float"            ;-> audio-wpctl
   "sbcl-cl-ppcre"               ;->
   "sbcl-stumpwm-ttf-fonts"      ;->
   "sbcl-stumpwm-kbd-layouts"    ;->
   "sbcl-stumpwm-swm-gaps"       ;->
   "sbcl-stumpwm-globalwindows"  ;-> need to properly configure
   "sbcl-stumpwm-screenshot"     ;-> preliminarily working
   ;"sbcl-stumpwm-end-session"   ;-> Not available in guix...
   ;; mode-line support
   "sbcl-stumpwm-cpu"
   "sbcl-stumpwm-mem"
   "sbcl-stumpwm-wifi"
   "sbcl-stumpwm-battery-portable"))

(define %emacs-packages ;-> perhaps move to a manifest
  (list
   "emacs"
   "emacs-diminish"
   "emacs-delight"
   "emacs-nord-theme"
   "emacs-doom-themes"
   "emacs-nerd-icons"
   "emacs-doom-modeline"
   "emacs-ligature"
   "emacs-no-littering"
   "emacs-ws-butler"
   "emacs-undo-tree"
   "emacs-paredit"
   "emacs-visual-fill-column"
   "emacs-mct"
   "emacs-orderless"
   "emacs-corfu"
   "emacs-marginalia"
   "emacs-beframe"
   "emacs-denote"
   "emacs-magit"
   "emacs-vterm"
   "emacs-guix"
   "emacs-arei"
   "emacs-sly"
   "emacs-nyxt"
   "emacs-stumpwm-mode"
   "emacs-mbsync"
   "emacs-org-superstar"
   "emacs-org-appear"
   "emacs-erc-hl-nicks"
   "emacs-erc-image"
   "emacs-emojify"
   "emacs-bongo"))

(home-environment
 ;; Below is the list of packages that will show up in your
 ;; Home profile, under ~/.guix-home/profile.
 (packages (specifications->packages
            (append
             %logoraz-packages
             %dev-packages
             %xorg-util-packages
             %stumpwm-packages
             %emacs-packages)))
 
 ;; Below is the list of Home services.  To search for available
 ;; services, run 'guix home search KEYWORD' in a terminal.
 ;; TODO: Implement the following
 ;; 1. (service home-xdg-configuration-files-service-type ...)
 ;; 2. (service home-files-service-type ...)
 (services
  (list
   (simple-service 'env-vars home-environment-variables-service-type
                   '(("EDITOR" . "emacs")
                     ("BROWSER" . "nyxt")
                     ("XDG_SESSION_TYPE" . "x11")
                     ("XDG_SESSION_DESKOP" . "stumpwm")
                     ("XDG_CURRENT_DESKTOP" . "stumpwm")
                     ("XDG_DOWNLOAD_DIR" . "/home/logoraz/Downloads/")))
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
              (list (local-file "./dot-bashrc.sh"
                                #:recursive? #t)))
             (bash-profile
              (list (local-file "./dot-bash_profile.sh"
                                #:recursive? #t))))))))
