(defpackage universal-tween-engine-demo
  (:use #:cl #:alexandria)
  (:nicknames #:ute.demo)
  (:export #:main))

(in-package #:universal-tween-engine-demo)

(declaim (inline integer-float))
(defun integer-float (integer)
  (coerce integer 'single-float))

(define-setf-expander integer-float (integer &environment env)
  (multiple-value-bind (vars vals newval setter getter) (get-setf-expansion integer env)
    (declare (ignore newval setter))
    (with-gensyms (store)
      (values vars vals `(,store) `(setf ,getter (values (floor ,store))) `(integer-float ,getter)))))

(defstruct sprite
  (position (raylib:vector2-zero) :type raylib:vector2)
  (size 20.0 :type single-float)
  (color (raylib:copy-color raylib:+black+) :type raylib:color))

(defun sprite-draw (sprite)
  (raylib:draw-circle-v
   (sprite-position sprite)
   (sprite-size sprite)
   (sprite-color sprite)))

(defmacro with-tween-loop ((tween alpha) &body body)
  (with-gensyms (delta time new-time loop-name)
    `(progn
       (ute:start
        (ute:tween
         :from ((,alpha) (0.0))
         :duration 0.5
         :callback (curry #'ute:start ,tween)))
       (loop :named ,loop-name
             :with ,time := (coerce (raylib:get-time) 'single-float)
             :and ,delta := 0.0
             :when (or (raylib:window-should-close) (raylib:is-key-pressed #.(cffi:foreign-enum-value 'raylib:keyboard-key :q)))
               :do (throw 'exit nil)
             :when (ute:killedp tween)
               :do (return-from ,loop-name)
             :when (and (ute:startedp tween)
                        (or (raylib:is-key-pressed #.(cffi:foreign-enum-value 'raylib:keyboard-key :space))
                            (raylib:is-mouse-button-down #.(cffi:foreign-enum-value 'raylib:mouse-button :left)))
                        (not (or (ute:pausedp tween) (ute:killedp tween))))
               :do (progn
                     (ute:pause ,tween)
                     (ute:start
                      (ute:tween
                       :to ((,alpha) (0.0))
                       :duration 0.5
                       :callback (curry #'ute:kill ,tween))))
             :do (progn
                   (let ((,new-time (coerce (raylib:get-time) 'single-float)))
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
              :for sprite := (make-sprite :size sprite-size :position (raylib:make-vector2 :x (+ (coerce x 'single-float) 200.0)
                                                                                           :y (coerce y 'single-float)))
              :collect (list (symbol-name easing)
                             (truncate x)
                             (truncate (- y (truncate font-size 2)))
                             font-size raylib:+black+)
                :into texts
              :collect sprite :into sprites
              :collect (let ((sprite sprite) (easing easing) (x-target x-target))
                         (ute:timeline
                          (:sequence
                           (:to (((integer-float (raylib:color-a (sprite-color sprite)))) (255.0))
                            :duration 0.5)
                           (:to (((raylib:vector2-x (sprite-position sprite))) (x-target))
                            :duration 1.0
                            :ease (fdefinition easing))
                           (:to (((integer-float (raylib:color-a (sprite-color sprite)))) (0.0))
                            :duration 0.5))))
                :into tweens
              :finally (return (values texts sprites tweens)))
      (let ((tween (ute:timeline (:parallel) :repeat (:count t))))
        (mapc (curry #'ute::timeline-add-child tween) tweens)
        (with-tween-loop (tween alpha)
          (raylib:with-drawing
            (raylib:clear-background raylib:+raywhite+)
            (loop :for sprite :in sprites
                  :for text :in texts
                  :unless (ute:startedp tween)
                    :do (setf (raylib:color-a (sprite-color sprite)) 0)
                  :when (ute:pausedp tween)
                    :do (setf (raylib:color-a (sprite-color sprite)) (floor (* alpha 255.0)))
                  :do (apply #'raylib:draw-text text)
                      (sprite-draw sprite))))))))

(defun timeline-example ()
  (let ((start-x (ftruncate +window-width+ 5))
        (start-y (ftruncate +window-height+ 5))
        (target-x (* (ftruncate +window-width+ 5) 4))
        (target-y (* (ftruncate +window-height+ 5) 4)))
    (let* ((sprite (make-sprite :position (raylib:make-vector2 :x start-x :y start-y)
                                :size 40.0))
           (tween (ute:timeline
                   (:sequence
                    (:parallel
                     (:to (((raylib:vector2-x (sprite-position sprite)) (raylib:vector2-y (sprite-position sprite)))
                           (target-x start-y)
                           (start-x target-y)
                           (target-x target-y))
                      :duration 3.0
                      :ease #'ute:quad-inout)
                     (:to (((integer-float (raylib:color-a (sprite-color sprite)))) (0.0) (255.0))
                      :path #'ute:linear
                      :duration 3.0
                      :ease #'ute:quad-inout))
                    (:parallel
                     (:to (((raylib:vector2-x (sprite-position sprite)) (raylib:vector2-y (sprite-position sprite)))
                           ((/ (- start-x target-x) 2) (/ (- start-y target-y) 2)))
                      :relativep t
                      :duration 1.0
                      :ease #'ute:expo-inout)
                     (:to (((sprite-size sprite)) (100.0))
                      :duration 1.0
                      :ease #'ute:expo-inout)
                     (:to (((integer-float (raylib:color-r (sprite-color sprite)))
                            (integer-float (raylib:color-b (sprite-color sprite)))
                            (integer-float (raylib:color-g (sprite-color sprite))))
                           ((* 0.8 255.0) (* 0.2 255.0) 0.0)
                           (0.0 (* 0.8 255.0) (* 0.2 255.0))
                           ((* 0.2 255.0) 0.0 (* 0.8 255.0))
                           ((* 0.5 255.0) (* 0.5 255.0) (* 0.5 255.0)))
                      :duration 1.5
                      :path #'ute:linear
                      :ease #'ute:quad-inout)))
                   :repeat (:count t :yoyop t))))
      (with-tween-loop (tween (integer-float (raylib:color-a (sprite-color sprite))))
        (raylib:with-drawing
          (raylib:clear-background raylib:+raywhite+)
          (raylib:draw-text "TIMELINE + WAYPOINTS" 20 20 40 raylib:+black+)
          (sprite-draw sprite))))))

(defun main ()
  (catch 'exit
    (unwind-protect
         (raylib:with-window ("Universal Tween Engine Demo" (+window-width+ +window-height+))
           (raylib:set-target-fps 60)
           (loop
             (easing-functions)
             (timeline-example)))
      (ute:kill-all)
      (ute:update 0.0))))
