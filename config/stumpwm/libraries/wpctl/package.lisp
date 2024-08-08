(defpackage :wpctl
  (:import-from :parse-float
                #:parse-float)
  (:import-from :ppcre
                #:create-scanner
                #:scan
                #:scan-to-strings)
  (:import-from :stumpwm
                #:defcommand
                #:add-screen-mode-line-formatter
                #:format-expand
                #:bar
                #:add-screen-mode-line-formatter
                #:format-with-on-click-id
                #:decode-button-code
                #:update-all-mode-lines
                #:register-ml-on-click-id)
  (:use #:cl)
  (:export #:volume-up
           #:volume-down
           #:set-volume
           #:get-volume
           #:get-mute
           #:mute
           #:unmute
           #:toggle-mute
           #:modeline
           #:ml-bar
           #:ml-volume
           #:*default-sink-id*
           #:*default-source-id*
           #:*mixer-command*
           #:*wpctl-path*
           #:*step*
           #:*modeline-fmt*
           #:*source-modeline-fmt*))
