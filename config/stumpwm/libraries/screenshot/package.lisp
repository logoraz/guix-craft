;;;; package.lisp --> screenshot

(defpackage #:screenshot
  (:import-from :local-time
                #:now
                #:format-timestring)
  (:import-from :xlib
                #:screen-default-colormap
                #:lookup-color
                #:alloc-color
                #:drawable-height
                #:drawable-width
                #:get-raw-image
                #:display-byte-order
                #:drawable-display
                #:create-window
                #:screen-root
                #:display-default-screen
                #:create-gcontext
                #:map-window
                #:grab-pointer
                #:event-case
                #:draw-rectangle
                #:global-pointer-position
                #:ungrab-pointer
                #:destroy-window)
  (:import-from :zpng
                #:start-png
                #:finish-png
                #:write-pixel
                #:pixel-streamed-png)
  (:import-from :stumpwm
                #:getenv
                #:defcommand
                #:current-screen
                #:current-window
                #:screen-number
                #:screen-width
                #:screen-height
                #:echo
                #:window-xwin
                #:*display*)
  (:use #:cl)
  (:export #:screenshot
           #:screenshot-window
           #:screenshot-area))
