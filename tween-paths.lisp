(in-package #:universal-tween-engine)

(declaim (ftype (function (f32 f32 f32 f32 f32) f32) tween-path-catmull-rom-spline))
(defun tween-path-catmull-rom-spline (a b c d x)
  (let ((t1 (* (- c a) 0.5))
        (t2 (* (- d b) 0.5))
        (h1 (+ (- (* +2.0 x x x) (* 3.0 x x)) 1.0))
        (h2 (+ (* -2.0 x x x) (* 3.0 x x)))
        (h3 (+ (- (* x x x) (* 2.0 x x)) x))
        (h4 (- (* x x x) (* x x))))
    (+ (* b h1) (* c h2) (* t1 h3) (* t2 h4))))

(declaim (ftype (function (f32 (simple-array f32 (*)) i32) f32) tween-path-catmull-rom))
(defun tween-path-catmull-rom (x points points-count)
  (declare (type f32 x))
  (let ((segment (clamp (floor (the i32->f32 (* (coerce (1- points-count) 'f32) x))) 0 (- points-count 2))))
    (setf x (- (* x (coerce (1- points-count) 'f32)) segment))
    (switch (segment :test #'=)
      (0
       (tween-path-catmull-rom-spline
        (aref points 0)
        (aref points 0)
        (aref points 1)
        (aref points 2)
        x))
      ((- points-count 2)
       (tween-path-catmull-rom-spline
        (aref points (- points-count 3))
        (aref points (- points-count 2))
        (aref points (- points-count 1))
        (aref points (- points-count 1))
        x))
      (t
       (tween-path-catmull-rom-spline
        (aref points (- segment 1))
        (aref points (- segment 0))
        (aref points (+ segment 1))
        (aref points (+ segment 2))
        x)))))

(declaim (ftype (function (f32 (simple-array f32 (*)) i32) f32) tween-path-linear))
(defun tween-path-linear (x points points-count)
  (declare (type f32 x))
  (let ((segment (clamp (floor (the i32->f32 (* (coerce (1- points-count) 'f32) x))) 0 (- points-count 2))))
    (setf x (- (* x (coerce (1- points-count) 'f32)) segment))
    (+ (aref points segment) (* x (- (aref points (1+ segment)) (aref points segment))))))
