;;;; screenshot.lisp

(in-package :screenshot)

(defparameter *screenshot-directory* (concatenate 'string
                                                  (getenv "HOME")
                                                  "/Pictures/Screenshots/")
  "Set default directory to save screenshots.")

(defun filename ()
  "Generate formated filename using PATH."
  (concatenate 'string
               *screenshot-directory*
               (format nil "screenshot__~a"
                       (format-timestring
                        nil
                        (now)
                        :format '(:year "-" :month "-" :day "-T"
                                  :hour "-" :min   "-" :sec)))
               ".png"))

;;TODO - Refactor the below code...
(defun colorname-to-color (colorname)
  (let* ((screen (current-screen))
         (colormap (screen-default-colormap (screen-number screen)))
         (color (lookup-color colormap colorname)))
    (alloc-color colormap color)))

;;FIXME - need to implement (sleep n) before is captures drawable so that it
;;        doesn't capture any of stumpwm's prompts
;;        May need to write a function that wraps drawable...
(defun capture (drawable file &key (x 0)
                                   (y 0)
                                   (height (drawable-height drawable))
                                   (width (drawable-width drawable)))
  "Captures the provided drawable area to file."
  (let* ((png (make-instance 'pixel-streamed-png
                             :color-type :truecolor-alpha
                             :width width
                             :height height)))
    (multiple-value-bind (pixarray depth visual)
        (get-raw-image drawable :x x :y y :width width :height height
                                     :format :Z-PIXMAP)
      (declare (ignore depth visual))
      (with-open-file (stream file
                              :direction :output
                              :if-exists :supersede
                              :if-does-not-exist :create
                              :element-type '(unsigned-byte 8))
        (start-png png stream)
        (case (display-byte-order (drawable-display drawable))
          (:lsbfirst
           (do ((i 0 (+ 4 i)))
               ((>= i (length pixarray)))
             (write-pixel (list (aref pixarray (+ 2 i))
                                (aref pixarray (+ 1 i))
                                (aref pixarray i)
                                #xFF)
                          png)))
          (:msbfirst
           (do ((i 0 (+ 4 i)))
               ((>= i (* height width 4)))
             (write-pixel (list (aref pixarray (1+ i))
                                (aref pixarray (+ 2 i))
                                (aref pixarray (+ 3 i))
                                #xFF)
                          png))))
        (finish-png png)))))

(defun clamp-xy (x1 y1 x2 y2)
  (values (max x1 0)
          (max y1 0)
          (max (+ 2 x1) x2)
          (max (+ 2 y1) y2)))

(defcommand screenshot-area () ()
  "Make screenshot of selected area of display."
  (let ((display *display*)
        (x1 0)
        (y1 0)
        (x2 0)
        (y2 0))
    ;; The secret to drawing the selection rectangle comes from the xor function
    ;; in the graphics context and the subwindow-mode :include-inferiors.
    ;; https://askubuntu.com/questions/487725/ \
    ;; how-can-i-draw-selection-rectangle-on-screen-for-my-script
    (let* ((window
             (create-window
              :parent (screen-root (display-default-screen display))
              :x 0
              :y 0
              :width (screen-width (current-screen))
              :height (screen-height (current-screen))
              :background :none
              :event-mask '(:exposure :button-press :button-release)))
           (gc (create-gcontext
                :drawable window
                :line-width 1
                :foreground (colorname-to-color "green")
                :function boole-xor
                :subwindow-mode :include-inferiors)))
      (unwind-protect
           (progn
             (map-window window)
             (grab-pointer
              window
              '(:button-press :button-release :button-motion) :owner-p t)
             (echo "Click and drag the area to screenshot.")
             (event-case (display :discard-p t)
               (exposure
                ()
                nil #| continue receiving events |#)
               (motion-notify
                (root-x root-y)
                (multiple-value-bind
                      (x1 y1 root-x root-y) (clamp-xy x1 y1 root-x root-y)
                  ;; Since we're using that boole-xor function in the graphics
                  ;; context, drawing over the old rectangle reverts the pixels
                  ;; back to their original values.
                  (when x2
                    (draw-rectangle
                     window gc x1 y1 (- x2 x1) (- y2 y1)))
                  (setf x2 root-x)
                  (setf y2 root-y)
                  ;; Now draw the new rectangle.
                  (draw-rectangle
                   window gc x1 y1 (- root-x x1) (- root-y y1)))
                nil)
               (button-press
                ()
                (multiple-value-bind
                      (root-x root-y) (global-pointer-position display)
                  (echo (format nil
                                "Screenshotting from ~A, ~A to ..."
                                root-x root-y))
                  (setf x1 root-x)
                  (setf y1 root-y)
                  (setf x2 (+ 1 x1))
                  (setf y2 (+ 1 y1))
                  (draw-rectangle window gc x1 y1 (- x2 x1) (- y2 y1))
                  nil))
               (button-release
                ()
                (multiple-value-bind
                      (root-x root-y) (global-pointer-position display)
                  (multiple-value-bind
                        (x1 y1 root-x root-y) (clamp-xy x1 y1 root-x root-y)
                    ;; Since we're using that boole-xor function in the graphics
                    ;; context, drawing over the old rectangle reverts the pixels
                    ;; back to their original values.
                    (when x2
                      (draw-rectangle
                       window gc x1 y1 (- x2 x1) (- y2 y1)))
                    (echo (format nil
                                  "Screenshotted from ~A, ~A to ~A, ~A to ~A"
                                  x1 y1 root-x root-y (filename)))
                    (capture (screen-root (screen-number (current-screen)))
                             (filename)
                             :x (- x1 1)
                             :y (- y1 1)
                             :width (- (- root-x x1) 1)
                             :height (- (- root-y y1) 1))
                    (ungrab-pointer display)
                    (destroy-window window))
                  t))))
        (destroy-window window)))))

(defcommand screenshot () ()
  "Make screenshot of root window"
  (capture (screen-root (screen-number (current-screen)))
           (filename))
    (echo "Captured current screen"))

(defcommand screenshot-window () ()
  "Make screenshot of focus window"
  (capture (window-xwin (current-window))
           (filename))
  (echo "Captured current window."))
