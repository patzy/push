;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
(in-package #:push)

(defvar *screen-width* 1024)
(defvar *screen-height* 768)
(defvar *view* (glaw:create-2d-view 0 0 *screen-width* *screen-height*))
(defvar *font* nil)

;;; Main code
(defun init ()
  (glaw:init-content-manager #P"./")
  (glaw:load-asset "dejavu-sans.fnt" :fonttool-bitmap-font "font")
  (setf *font* (glaw:use-resource "font")))

(defun shutdown ()
  (glaw:dispose-asset "font")
  (glaw:shutdown-content-manager))

(defun draw ()
  (glaw:set-view-2d *view*)
  (glaw:begin-draw)
  (glaw:end-draw))

(let ((last-update-time (get-internal-real-time)))
  (defun idle ()
    (let* ((elapsed-time (- (get-internal-real-time)
                            last-update-time))
           (dt (/ (* elapsed-time 1.0)
                  internal-time-units-per-second)))
      (setf last-update-time (get-internal-real-time)))))

(defmethod glop:on-key (window pressed keycode keysym string)
  (glaw:dispatch-key-event keysym (if pressed :press :release) keycode string)
  (when (eql keysym :escape)
    (glop:push-close-event window)))

(defmethod glop:on-close (window)
  (shutdown))

(defmethod glop:on-button (window pressed button)
  (glaw:dispatch-button-event :mouse (glaw:translate-mouse-button button)
                              (if pressed :press :release)))

(defmethod glop:on-mouse-motion (window x y dx dy)
  (glaw:update-mouse-position x y)
  (glaw:dispatch-motion-event :mouse dx dy))

(defmethod glop:on-draw (window)
  (draw)
  (glop:swap-buffers window))

(defmethod glop:on-resize (window w h)
  (glaw:reshape w h)
  (draw)
  (glop:swap-buffers window))


(defun run ()
  ;; how to get extensions
  (setf cl-opengl-bindings:*gl-get-proc-address* 'glop:gl-get-proc-address)
  (glop:with-window (win "Push" 800 600)
    (glaw:setup-gl-defaults)
    (glaw:reshape 800 600)
    (init)
    (loop while (glop:dispatch-events win :blocking nil) do
         (idle)
         (draw)
         (glop:swap-buffers win))))

