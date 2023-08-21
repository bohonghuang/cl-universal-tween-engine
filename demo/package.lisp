(defpackage universal-tween-engine-demo
  (:use #:cl #:alexandria #:raylib #:3d-vectors)
  (:nicknames #:ute.demo)
  (:export #:main))

(in-package #:universal-tween-engine-demo)

(defstruct color
  (r 0.0 :type single-float)
  (g 0.0 :type single-float)
  (b 0.0 :type single-float)
  (a 1.0 :type single-float))

(defun color-rgba (color)
  (raylib::make-rgba
   (floor (* (color-r color) #xFF))
   (floor (* (color-g color) #xFF))
   (floor (* (color-b color) #xFF))
   (floor (* (color-a color) #xFF))))

(defstruct sprite
  (position (vec 0.0 0.0) :type vec2)
  (size 20.0 :type single-float)
  (color (make-color) :type color))

(defun sprite-draw (sprite)
  (draw-circle-v (sprite-position sprite)
                 (sprite-size sprite)
                 (color-rgba (sprite-color sprite))))

(defmacro with-tween-loop ((tween alpha) &body body)
  (with-gensyms (delta time new-time loop-name)
    `(progn
       (ute:start
        (ute:tween
         :from ((,alpha) (0.0))
         :duration 0.5
         :callback (curry #'ute:start ,tween)))
       (loop :named ,loop-name
             :with ,time := (coerce (get-time) 'single-float)
             :and ,delta := 0.0
             :when (or (window-should-close) (is-key-pressed 81))
               :do (throw 'exit nil)
             :when (ute:killedp tween)
               :do (return-from ,loop-name)
             :when (and (ute:startedp tween)
                        (or (is-key-pressed 32)
                            (is-mouse-button-down 0))
                        (not (or (ute:pausedp tween) (ute:killedp tween))))
               :do (progn
                     (ute:pause ,tween)
                     (ute:start
                      (ute:tween
                       :to ((,alpha) (0.0))
                       :duration 0.5
                       :callback (curry #'ute:kill ,tween))))
             :do (progn
                   (let ((,new-time (coerce (get-time) 'single-float)))
                     (setf ,delta (- ,new-time ,time)
                           ,time ,new-time))
                   (ute:update ,delta)
                   ,@body)))))

(defconstant +window-width+ 1280)
(defconstant +window-height+ 720)

(defun easing-functions ()
  (let ((alpha 1.0))
    (multiple-value-bind (texts sprites tweens)
        (loop :with y-step := (/ +window-height+ (1+ (truncate (1+ (length ute:+easings+)) 2)))
              :with font-size := 20 :and sprite-size := 20.0
              :for easing :in ute:+easings+
              :for index :from 0
              :for x := y-step :then (+ x (if (= (truncate (length ute:+easings+) 2) index)
                                              (ftruncate +window-width+ 2.0) 0.0))
              :for x-target := (+ x (- (ftruncate +window-width+ 2.0) (* y-step 2)))
              :for y := y-step :then (if (= (truncate (length ute:+easings+) 2) index) y-step (+ y y-step))
              :for sprite := (make-sprite :size sprite-size :position (vec (+ x 200.0) y))
              :collect (list (symbol-name easing)
                             (truncate x)
                             (truncate (- y (truncate font-size 2)))
                             font-size +black+)
                :into texts
              :collect sprite :into sprites
              :collect (let ((sprite sprite) (easing easing) (x-target x-target))
                         (ute:timeline
                          (:sequence
                           (:to (((color-a (sprite-color sprite))) (1.0))
                            :duration 0.5)
                           (:to (((vx (sprite-position sprite))) (x-target))
                            :duration 1.0
                            :ease (fdefinition easing))
                           (:to (((color-a (sprite-color sprite))) (0.0))
                            :duration 0.5))))
                :into tweens
              :finally (return (values texts sprites tweens)))
      (let ((tween (ute:timeline (:parallel) :repeat (:count t))))
        (mapc (curry #'ute::timeline-add-child tween) tweens)
        (with-tween-loop (tween alpha)
          (with-drawing
            (clear-background +raywhite+)
            (loop :for sprite :in sprites
                  :for text :in texts
                  :unless (ute:startedp tween)
                    :do (setf (color-a (sprite-color sprite)) 0.0)
                  :when (ute:pausedp tween)
                    :do (setf (color-a (sprite-color sprite)) alpha)
                  :do (apply #'draw-text text)
                      (sprite-draw sprite))))))))

(defun timeline-example ()
  (let ((start-x (ftruncate +window-width+ 5))
        (start-y (ftruncate +window-height+ 5))
        (target-x (* (ftruncate +window-width+ 5) 4))
        (target-y (* (ftruncate +window-height+ 5) 4)))
    (let* ((sprite (make-sprite :position (vec start-x start-y) :size 40.0))
           (tween (ute:timeline
                   (:sequence
                    (:parallel
                     (:to (((vx (sprite-position sprite)) (vy (sprite-position sprite)))
                           (target-x start-y)
                           (start-x target-y)
                           (target-x target-y))
                      :duration 3.0
                      :ease #'ute:quad-inout)
                     (:to (((color-a (sprite-color sprite))) (0.0) (1.0))
                      :path #'ute:linear
                      :duration 3.0
                      :ease #'ute:quad-inout))
                    (:parallel
                     (:to (((vx (sprite-position sprite)) (vy (sprite-position sprite)))
                           ((/ (- start-x target-x) 2) (/ (- start-y target-y) 2)))
                      :relativep t
                      :duration 1.0
                      :ease #'ute:expo-inout)
                     (:to (((sprite-size sprite)) (100.0))
                      :duration 1.0
                      :ease #'ute:expo-inout)
                     (:to (((color-r (sprite-color sprite))
                            (color-b (sprite-color sprite))
                            (color-g (sprite-color sprite)))
                           (0.8 0.2 0.0)
                           (0.0 0.8 0.2)
                           (0.2 0.0 0.8)
                           (0.5 0.5 0.5))
                      :duration 1.5
                      :path #'ute:linear
                      :ease #'ute:quad-inout)))
                   :repeat (:count t :yoyop t))))
      (with-tween-loop (tween (color-a (sprite-color sprite)))
        (with-drawing
          (clear-background +raywhite+)
          (draw-text "TIMELINE + WAYPOINTS" 20 20 40 +black+)
          (sprite-draw sprite))))))

(defun main ()
  (catch 'exit
    (unwind-protect
         (with-window (+window-width+ +window-height+ "Universal Tween Engine Demo")
           (set-target-fps 60)
           (loop
             (easing-functions)
             (timeline-example)))
      (ute:kill-all)
      (ute:update 0.0))))
