;; Generated from: guix home import ~/Repos/guix-conf
;;
;; This home environment, "home.scm", file can be passed to 'guix home reconfigure'
;; to reproduce the content of your profile.  This is "symbolic": it only
;; specifies package names.  To reproduce the exact same profile, you also
;; need to capture the channels being used, as returned by "guix describe".
;; See the "Replicating Guix" section in the manual.

(use-modules (gnu home)
             (gnu packages)
             (gnu services)
             (guix gexp)
             (gnu home services shells))

(home-environment
 ;; Below is the list of packages that will show up in your
 ;; Home profile, under ~/.guix-home/profile.
 (packages
  (specifications->packages
   (list "libnotify"
         "dunst"
         "blueman"
         "pasystray"
         "polybar"
         "picom"
         "nm-tray"
         "icecat"
         "emacs"
         "gnucash"
         "vlc"
         "gimp"
         "gst-plugins-good"
         "gst-plugins-bad"
         "gst-libav"
         "lm-sensors"
         "emacs-guix"
         "font-google-material-design-icons"
         "xset"
         "guile-ares-rs"
         "guile-next"
         "emacs-arei"
         "guile-hoot"
         "alsa-utils"
         "xss-lock"
         "slock"
         "playerctl"
         "brightnessctl"
         "scrot"
         "feh"
         "emacs-beframe"
         "emacs-corfu"
         "mu"
         "pinentry"
         "inkscape"
         "emacs-magit"
         "msmtp"
         "git"
         "emacs-org-superstar"
         "emacs-vterm"
         "texlive-latex"
         "texlive-collection-latex"
         "texlive"
         "gnupg"
         "gstreamer"
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
         "xrdb"
         "xcursor-themes"
         "xrandr"
         "gdb"
         "guile"
         "isync"
         "font-fira-code"
         "font-iosevka-aile")))

  ;; Below is the list of Home services.  To search for available
  ;; services, run 'guix home search KEYWORD' in a terminal.
  (services
   (list
    (service home-bash-service-type
             (home-bash-configuration
              (aliases '(("grep" . "grep --color=auto") ("ll" . "ls -l")
                         ("ls" . "ls -p --color=auto")))
              (bashrc (list (local-file "./.bashrc" "bashrc")))
              (bash-profile (list (local-file "./.bash_profile"
                                              "bash_profile"))))))))
