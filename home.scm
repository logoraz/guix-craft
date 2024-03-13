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

(home-environment
  ;; Below is the list of packages that will show up in your
  ;; Home profile, under ~/.guix-home/profile.
  (packages (specifications->packages
             (list
              "pavucontrol"
              "nm-tray"
              "stumpish"
              "ccl"                      ;; core
              "sbcl"                     ;; core
              "sbcl-stumpwm-stumptray"
              "sbcl-stumpwm-swm-gaps"
              "sbcl-stumpwm-kbd-layouts"
              "sbcl-stumpwm-ttf-fonts"
              "emacs-nyxt"
              "nyxt"
              "dunst"
              "blueman"
              "pasystray"
              "icecat"
              "gnucash"
              "vlc"
              "gimp"
              "gst-plugins-good"
              "gst-plugins-bad"
              "gst-libav"
              "mu"
              "lm-sensors"
              "emacs-stumpwm-mode"
              "make"
              "emacs-guix"
              "libnotify"
              "picom"
              "emacs"                    ;; core
              "xset"
              "xinput"                   ;; new - optional
              "libxi"                    ;; new - optional
              "guile-ares-rs"
              "alsa-utils"
              "xss-lock"
              "slock"
              "playerctl"
              "brightnessctl"
              "scrot"
              "feh"
              "pinentry"
              "inkscape"
              "emacs-magit"
              "msmtp"
              "git"                      ;; core
              "emacs-vterm"
              "texlive-latex"
              "texlive-collection-latex"
              ;; "texlive"
              "gnupg"
              "gstreamer"
              "xrdb"
              "xrandr"
              "gdb"
              "isync"
              "curl"
              "guile"                    ;; core
              "emacs-sly"
              "emacs-corfu"
              "emacs-org-superstar"
              "font-google-material-design-icons"
              "emacs-arei"
              "guile-hoot"
              "emacs-beframe"
              "emacs-marginalia"
              "emacs-denote"
              "emacs-paredit"
              "emacs-mbsync"
              "emacs-no-littering"
              "emacs-org-appear"
              "emacs-orderless"
              "emacs-mct"
              "emacs-bongo"
              "emacs-ligature"
              "emacs-undo-tree"
              "emacs-ws-butler"
              "emacs-nord-theme"
              "xcursor-themes"
              "font-fira-code"
              "font-iosevka-aile")))

  ;; Below is the list of Home services.  To search for available
  ;; services, run 'guix home search KEYWORD' in a terminal.
  (services
   (list (service home-bash-service-type
                  (home-bash-configuration
                   (aliases '(("grep" . "grep --color=auto") ("ll" . "ls -l")
                              ("ls" . "ls -p --color=auto")))
                   (bashrc (list (local-file ".bashrc" "bashrc")))
                   (bash-profile (list (local-file ".bash_profile"
                                                   "bash_profile"))))))))
