;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
(in-package #:push)

(defparameter *screens* (glaw:make-screen-stack))
(defparameter *debug-view* (glaw:create-2d-view 0 -300 1024 600))

;;; Game entities

(defstruct wall
  texture
  shape
  bbox)

(defun create-wall (x y)
  (let ((wall (make-wall :texture (glaw:use-resource "metal-tex")
                         :bbox (glaw:make-bbox)
                         :shape (glaw:create-rectangle-shape x y (+ x 208) (+ y 50)
                                                             :filled t)
                         )))
    (glaw:bbox-update/shape (wall-bbox wall) (wall-shape wall))
    wall))

(defun render-wall (wall)
  (glaw:select-texture (wall-texture wall))
  (glaw:render-shape (wall-shape wall)))

(defstruct particle
  (holded t)
  (teleport nil)
  x y
  x-off y-off
  sprite
  bbox
  (vx 500.0))

(defun create-particle (x y)
  (let ((part (make-particle
               :x x :y y
               :x-off 0 :y-off 0
               :sprite (glaw:create-sprite x y 50 50 (glaw:use-resource "particle-sprite"))
               :bbox (glaw:make-bbox))))
    (glaw:bbox-update/shape (particle-bbox part) (glaw::sprite-shape (particle-sprite part)))
    part))

(defun particle-start-teleport (part)
  (setf (particle-teleport part) 0.0))

(defun particle-update-teleport (part dt)
  (incf (particle-teleport part) dt)
  (when (> (particle-teleport part) 1.0)
    (setf (particle-teleport part) 1.0)))

(defun particle-end-teleport (part)
  (let ((dt (particle-teleport part)))
    ;; remove some particle energy
    (decf (particle-vx part) (* dt (particle-vx part))))
  (setf (particle-teleport part) nil))

(defun render-particle (part)
  (let ((jitter (/ (particle-vx part) 300)))
      (gl:with-pushed-matrix
        (unless (zerop jitter)
          (gl:translate (glaw:random-between (- jitter) jitter)
                        (glaw:random-between (- jitter) jitter)
                        0))
      (glaw:render-sprite (particle-sprite part))))
  (when (particle-teleport part)
    (let ((dx (* (particle-teleport part) (particle-vx part))))
      (glaw:translate-shape (glaw::sprite-shape (particle-sprite part)) dx 0)
      (glaw:set-color/rgb 0.0 1.0 0.0 0.6)
      (glaw:render-sprite (particle-sprite part))
      (glaw:translate-shape (glaw::sprite-shape (particle-sprite part)) (- dx) 0))))

(let ((acc 200.0)
      (max-speed 5000.0))
  (defun update-particle (part dt)
    (incf (particle-vx part) (* dt acc))
    (when (> (particle-vx part) max-speed)
      (setf (particle-vx part) max-speed))
    (when (particle-teleport part)
      (particle-update-teleport part dt))))

(defun move-particle (part delta)
  (incf (particle-x-off part) delta)
  (glaw:translate-shape (glaw::sprite-shape (particle-sprite part)) delta 0)
  (glaw:bbox-translate (particle-bbox part) delta 0))

(defun particle-collide-p (part obs)
  (glaw:bbox-intersect-p (particle-bbox part) (obstacle-bbox obs)))

(defstruct obstacle
  pos width
  texture
  shape
  bbox
  on-collision
  collision-args)

(defun obstacle-end (obs)
  (+ (obstacle-pos obs) (obstacle-width obs)))

(defun create-obstacle (x width keys)
  (let ((obs (apply #'make-obstacle :bbox (glaw:make-bbox)
                                    :shape (glaw:create-rectangle-shape x -300 (+ x width) 300
                                                                        :filled t)
                                    :pos x :width width
                                    :allow-other-keys t keys)))
    (glaw:bbox-update/shape (obstacle-bbox obs) (obstacle-shape obs))
    obs))

(defun render-obstacle (obstacle)
  (glaw:select-texture (obstacle-texture obstacle))
  (glaw:render-shape (obstacle-shape obstacle)))

;;; Game Over screen
(defstruct game-over-screen
  (view (glaw:create-2d-view 0 0 1024 768))
  font)

(glaw:key-handler (it game-over-screen) (:escape :press)
     (glaw:empty-screen-stack *screens*))

(glaw:key-handler (it game-over-screen) (:space :press)
     (glaw:empty-screen-stack *screens*)
     (glaw:push-screen (make-game-screen) *screens*))

(defmethod glaw:init-screen ((it game-over-screen) &key)
  (setf (game-over-screen-font it) (glaw:use-resource "font"))
  (glaw:push-input-handlers)
  (glaw:add-input-handler it))

(defmethod glaw:shutdown-screen ((it game-over-screen))
  (glaw:drop-resource "font")
  (glaw:remove-input-handler it)
  (glaw:pop-input-handlers))

(defmethod glaw:render-screen ((it game-over-screen))
  (glaw:set-view-2d (game-over-screen-view it))
  (glaw:select-texture nil)
  (glaw:set-color/rgb 1 0 0 1)
  (glaw:render-wrapped-string 0 384 1024
                              (game-over-screen-font it)
                              "You lost. Press ESC key to continue or SPACE to retry."
                              :justify :center))

(defmethod glaw:update-screen ((it game-over-screen) dt)
  (declare (ignore it dt)))

;;; Countdown Screen
(defstruct countdown-screen
  (view (glaw:create-2d-view 0 0 1024 768))
  (remaining 5.0)
  font)

(defmethod glaw:init-screen ((it countdown-screen) &key)
  (setf (countdown-screen-font it) (glaw:use-resource "font")))

(defmethod glaw:shutdown-screen ((it countdown-screen))
  (glaw:drop-resource "font"))

(defmethod glaw:render-screen ((it countdown-screen))
  (glaw:set-view-2d (countdown-screen-view it))
  (glaw:select-texture nil)
  (glaw:set-color/rgb 1 0 0 1)
  (glaw:render-wrapped-string 0 (* (glaw:2d-view-height (countdown-screen-view it)) 0.6)
                              (glaw:2d-view-width (countdown-screen-view it))
                              (countdown-screen-font it)
                              (format nil "~A" (countdown-screen-remaining it))
                              :justify :center))

(defmethod glaw:update-screen ((it countdown-screen) dt)
  (decf (countdown-screen-remaining it) dt)
  (when (<= (countdown-screen-remaining it) 0.0) ;; game over
    (glaw:push-screen (make-game-over-screen) *screens*
                      :propagate-rendering t)))

;;; Play screen
(defstruct game-screen
  (score 0)
  (dead-acc 0.0)
  (dying nil)
  player
  (obstacles '())
  (walls '())
  (view (glaw:create-2d-view 0 -300 1024 600))
  font)

(let ((acc 0.0))
  (defun game-screen-scroll (scr dx)
    (incf acc dx)
    (let ((delta (floor acc)))
      (incf (game-screen-score scr) delta)
      (dolist (o (game-screen-obstacles scr))
        (glaw:translate-shape (obstacle-shape o) (- delta) 0)
        (glaw:bbox-translate (obstacle-bbox o) (- delta) 0)
        (decf (obstacle-pos o) delta))
      (dolist (w (game-screen-walls scr))
        (glaw:translate-shape (wall-shape w) (- delta) 0)
        (glaw:bbox-translate (wall-bbox w) (- delta) 0)
        (when (<= (glaw:bbox-x-max (wall-bbox w)) 0)
        (let ((width 2288)) ;; 11*208
          (glaw:translate-shape (wall-shape w) width 0)
          (glaw:bbox-translate (wall-bbox w) width 0))))
      (decf acc delta))))

(defun game-screen-update-view (scr dt)
  (let* ((speed (particle-vx (game-screen-player scr)))
         (dist-to-min (- 1.0 (glaw:2d-view-zoom (game-screen-view scr))))
         (dist-to-max (- (min (/ speed 500.0) 1.5) (glaw:2d-view-zoom (game-screen-view scr)))))
    (if (< speed 500.0)
        (glaw:zoom-2d-view (game-screen-view scr) (* dist-to-min dt) :lock-left t)
        (glaw:zoom-2d-view (game-screen-view scr) (* dist-to-max dt) :lock-left t))))

(defun game-screen-difficulty (scr)
  (+ (/ (particle-vx (game-screen-player scr)) 5000.0)
     (min (/ (game-screen-score scr) 1.0E5) 1.0)))

(defun die-on-collide (game-scr)
  (glaw:push-screen (make-game-over-screen) *screens*
                    :propagate-rendering t))

(defun slow-on-collide (game-scr amount)
  (decf (particle-vx (game-screen-player game-scr))
        (* (particle-vx (game-screen-player game-scr)) amount)))

(let ((last-obstacle nil)) ;; last obstacle generated
  (defun game-screen-update-obstacles (scr dt)
    (declare (ignore dt))
    (setf (game-screen-obstacles scr)
          (remove-if (lambda (obs)
                       (< (obstacle-end obs) 0.0)) (game-screen-obstacles scr)))
    (unless (and last-obstacle (> (+ (obstacle-pos last-obstacle) (obstacle-width last-obstacle))
                                  2000.0))
      (let ((obstacles (list (list :containment 0.1
                                   :texture (glaw:use-resource "containment-tex")
                                   :on-collision #'die-on-collide
                                   :collision-args (list scr))
                             (list :field 0.5
                                   :texture (glaw:use-resource "field-tex")
                                   :on-collision #'slow-on-collide
                                   :collision-args (list scr 0.1))
                             (list :warp 0.4
                                   :texture (glaw:use-resource "warp-tex")
                                   :on-collision #'slow-on-collide
                                   :collision-args (list scr -0.2)))))
        (let* ((difficulty (game-screen-difficulty scr))
               (end (if last-obstacle
                        (+ (obstacle-pos last-obstacle) (obstacle-width last-obstacle))
                        (glaw:2d-view-width (game-screen-view scr))))
               (next-obs (cddr (glaw:random-nth obstacles)))
               (max-width (* 0.2 (glaw:2d-view-width (game-screen-view scr))))
               (next-width (+ 10.0 (* difficulty max-width)))
               (next-pos (+ end (+ 500.0 (random (1+ (floor (* 10000.0 (- 1.0 difficulty)))))))))
        (setf last-obstacle (create-obstacle next-pos next-width next-obs))
        (push last-obstacle (game-screen-obstacles scr)))))))

(defun game-screen-check-death (scr dt)
  (let ((part (game-screen-player scr)))
    (if (< (particle-vx part) 500.0)
        (unless (game-screen-dying scr)
          (glaw:push-screen (make-countdown-screen :remaining 3.0) *screens*
                            :propagate-rendering t :propagate-updating t)
          (setf (game-screen-dying scr) t))
        (when (game-screen-dying scr)
          (glaw:pop-screen *screens*)
          (setf (game-screen-dying scr) nil)))))

(glaw:key-handler (it game-screen) (:space :press)
  (setf (particle-holded (game-screen-player it)) nil))

(glaw:key-handler (it game-screen) (:t :press)
  (particle-start-teleport (game-screen-player it)))

(glaw:key-handler (it game-screen) (:t :release)
  (move-particle (game-screen-player it)
             (* (particle-vx (game-screen-player it)) (particle-teleport (game-screen-player it))))
  (particle-end-teleport (game-screen-player it)))

(defmethod glaw:init-screen ((it game-screen) &key)
  (setf (game-screen-player it) (create-particle 100 0)
        (game-screen-font it) (glaw:use-resource "font"))
  (loop for i below 11
       with x = 0 do
       (push (create-wall x -325) (game-screen-walls it))
       (incf x 208))
  (loop for i below 11
       with x = 0 do
       (push (create-wall x 275) (game-screen-walls it))
       (incf x 208))
  (glaw:add-input-handler it))

(defmethod glaw:shutdown-screen ((it game-screen))
  (glaw:remove-input-handler it))

(defmethod glaw:update-screen ((it game-screen) dt)
  (game-screen-update-view it dt)
  (game-screen-update-obstacles it dt)
  (unless (zerop (particle-x-off (game-screen-player it)))
    (game-screen-scroll it (* (particle-x-off (game-screen-player it)) dt))
    (move-particle (game-screen-player it) (- (* (particle-x-off (game-screen-player it)) dt))))
  (unless (particle-holded (game-screen-player it))
    (game-screen-scroll it (* (particle-vx (game-screen-player it)) dt))
    (update-particle (game-screen-player it) dt))
  (dolist (o (game-screen-obstacles it))
    (when (and (particle-collide-p (game-screen-player it) o)
               (obstacle-on-collision o))
      (apply (obstacle-on-collision o) (obstacle-collision-args o))))
  (game-screen-check-death it dt))

(defmethod glaw:render-screen ((it game-screen))
  ;;(glaw:set-view-2d *current-view*)
  (glaw:set-view-2d (game-screen-view it))
  (glaw:select-texture nil)
  (glaw:set-color/rgb 1 1 1 1)
  (gl:begin :line-strip)
  (gl:vertex (glaw:2d-view-left (game-screen-view it)) (glaw:2d-view-top (game-screen-view it)))
  (gl:vertex (glaw:2d-view-right (game-screen-view it)) (glaw:2d-view-top (game-screen-view it)))
  (gl:vertex (glaw:2d-view-right (game-screen-view it)) (glaw:2d-view-bottom (game-screen-view it)))
  (gl:vertex (glaw:2d-view-left (game-screen-view it)) (glaw:2d-view-bottom (game-screen-view it)))
  (gl:vertex (glaw:2d-view-left (game-screen-view it)) (glaw:2d-view-top (game-screen-view it)))
  (gl:end)
  (glaw:set-color/rgb 0.3 0.5 0.8 1.0)
  (render-particle (game-screen-player it))
  (glaw:set-color/rgb 1.0 1.0 1.0 1.0)
  (dolist (o (game-screen-obstacles it))
    ;;(when (glaw:bbox-visible-p (wall-bbox w) (game-screen-view it))
      (render-obstacle o))
  ;;)
  (dolist (w (game-screen-walls it))
    ;(when (glaw:bbox-visible-p (wall-bbox w) (game-screen-view it))
      (render-wall w));)
  (glaw:set-color/rgb 1.0 1.0 1.0 1.0)
  (glaw:format-at 100 160 (game-screen-font it) "Difficulty: ~A" (game-screen-difficulty it))
  (glaw:format-at 100 140 (game-screen-font it) "SCORE: ~A" (game-screen-score it))
  (glaw:format-at 100 120 (game-screen-font it) "Speed: ~A" (particle-vx (game-screen-player it)))
  (glaw:format-at 100 100 (game-screen-font it) "FPS: ~A" (glaw:current-fps)))

;;; Main code
(defun init ()
  (glaw:init-content-manager #P"./")
  (glaw:load-asset "dejavu-sans.fnt" :fonttool-bitmap-font "font")
  (glaw:load-asset "particle.png" :texture "particle-sprite")
  (glaw:load-asset "metal.png" :texture "metal-tex")
  (glaw:load-asset "containment.png" :texture "containment-tex")
  (glaw:load-asset "field.png" :texture "field-tex")
  (glaw:load-asset "warp.png" :texture "warp-tex")
  (gl:clear-color 0 0 0 0)
  (glaw:push-screen (make-game-screen) *screens*))


(defun shutdown ()
  (glaw:empty-screen-stack *screens*)
  (glaw:dispose-asset "font")
  (glaw:dispose-asset "particle-sprite")
  (glaw:dispose-asset "metal-tex")
  (glaw:dispose-asset "containment-tex")
  (glaw:dispose-asset "field-tex")
  (glaw:dispose-asset "warp-tex")
  (glaw:shutdown-content-manager))

(defun draw (window)
  (glaw:begin-draw)
  (glaw:render-screens *screens*)
  (glaw:end-draw)
  (glop:swap-buffers window))

(defun update (dt)
  (glaw:update-scheduler dt)
  (glaw:update-screens *screens* dt))

(defmethod glop:on-key (window pressed keycode keysym string)
  (glaw:dispatch-key-event keysym (if pressed :press :release) keycode string)
  (when (eql keysym :escape)
    (glop:push-close-event window)))

(defmethod glop:on-close (window)
  (declare (ignore window))
  (shutdown))

(defmethod glop:on-button (window pressed button)
  (declare (ignore window))
  (glaw:dispatch-button-event :mouse (glaw:translate-mouse-button button)
                              (if pressed :press :release)))

(defmethod glop:on-mouse-motion (window x y dx dy)
  (declare (ignore window))
  (glaw:update-mouse-position x y)
  (glaw:dispatch-motion-event :mouse dx dy))

(defmethod glop:on-draw (window)
  (draw window))

(defmethod glop:on-resize (window w h)
  (glaw:reshape w h)
  (draw window))

(defun run ()
  ;; disable auto repeat
  (setf glop:*ignore-auto-repeat* t)
  ;; how to get extensions
  (setf cl-opengl-bindings:*gl-get-proc-address* 'glop:gl-get-proc-address)
  (glop:with-window (win "Push" 800 600)
    ;;(glop:set-fullscreen win)
    (glaw:setup-gl-defaults)
    (glaw:reshape 800 600)
    (init)
    (let ((last-update-time (get-internal-real-time)))
      (loop while (and *screens* (glop:dispatch-events win :blocking nil)) do
           (let* ((elapsed-time (- (get-internal-real-time) last-update-time))
                  (dt (float (/ elapsed-time internal-time-units-per-second))))
             (setf last-update-time (get-internal-real-time))
             (glaw:with-timestep (dt 0.01)
               (update dt)
               (draw win)))))))



(run)