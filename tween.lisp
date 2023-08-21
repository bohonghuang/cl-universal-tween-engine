(in-package #:universal-tween-engine)

(defconstant +tween-repeat-infinity+ -1)

(defconstant +tween-combined-attributes-limit+ 9)
(defconstant +tween-waypoints-limit+ 3)

(deftype tween-accessor () `(function ((simple-array f32 (*)))))

(defstruct (tween (:include base-tween))
  (values-getter #'values :type tween-accessor)
  (values-setter #'values :type tween-accessor)
  (equation #'tween-equation-linear-inout :type (function (f32) f32))
  (path #'tween-path-linear :type (function (f32 (simple-array f32 (*)) i32) f32))
  (fromp nil :type boolean)
  (relativep nil :type boolean)
  (combined-attributes-count 0 :type i32)
  (waypoints-count 0 :type i32)
  (start-values (make-array +tween-combined-attributes-limit+
                            :element-type 'f32)
   :type (simple-array f32 (*)))
  (target-values (make-array +tween-combined-attributes-limit+
                             :element-type 'f32)
   :type (simple-array f32 (*)))
  (waypoints (make-array (* +tween-combined-attributes-limit+
                            +tween-waypoints-limit+)
                         :element-type 'f32)
   :type (simple-array f32 (*)))
  (accessor-buffer (make-array +tween-combined-attributes-limit+
                               :element-type 'f32)
   :type (simple-array f32 (*)))
  (path-buffer (make-array (* +tween-combined-attributes-limit+
                              (+ +tween-waypoints-limit+ 2))
                           :element-type 'f32)
   :type (simple-array f32 (*))))

(declaim (ftype (function (tween)) tween-reset))
(defun tween-reset (self)
  (%base-tween-reset self)
  (setf (tween-values-getter self) #'values
        (tween-values-setter self) #'values
        (tween-equation self) #'tween-equation-linear-inout
        (tween-path self) #'tween-path-linear
        (tween-fromp self) nil
        (tween-relativep self) nil
        (tween-combined-attributes-count self) 0
        (tween-waypoints-count self) 0)
  (let ((accessor-buffer-size +tween-combined-attributes-limit+))
    (declare (type i32 accessor-buffer-size))
    (when (/= (length (tween-accessor-buffer self)) accessor-buffer-size)
      (setf (tween-accessor-buffer self) (make-array accessor-buffer-size :element-type 'f32))))
  (let ((path-buffer-size (* (+ +tween-waypoints-limit+ 2) +tween-combined-attributes-limit+)))
    (declare (type i32 path-buffer-size))
    (when (/= (length (tween-path-buffer self)) path-buffer-size)
      (setf (tween-path-buffer self) (make-array path-buffer-size :element-type 'f32)))))

(defmethod base-tween-reset ((self tween))
  (tween-reset self))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf (fdefinition 'tween-start) (fdefinition '%base-tween-start)))

(defmethod base-tween-start ((self tween))
  (tween-start self))

(defparameter *tween-pool* (make-pool :object-creator #'make-tween
                                      :callback (make-pool-callback :on-pool #'tween-reset
                                                                    :on-unpool #'tween-reset)))

(declaim (ftype (function (tween i32 tween-accessor tween-accessor f32)) tween-setup))
(defun tween-setup (self n getter setter duration)
  (when (minusp duration)
    (error "Duration can't be negative."))
  (when (> n +tween-combined-attributes-limit+)
    (error "Combined attribute limit is exceeded."))
  (setf (tween-combined-attributes-count self) n
        (tween-values-getter self) getter
        (tween-values-setter self) setter
        (tween-duration self) duration))

(declaim (ftype (function (tween &rest f32)) tween-set-target))
(defun tween-set-target (self &rest args)
  (loop :for arg :of-type f32 :in args
        :for i :of-type i32 :from 0
        :do (setf (aref (tween-target-values self) i) arg)))

(declaim (ftype (function (tween &rest f32)) tween-set-relative-target))
(defun tween-set-relative-target (self &rest args)
  (loop :initially (setf (tween-relativep self) t)
        :for arg :of-type f32 :in args
        :for i :of-type i32 :from 0
        :do (setf (aref (tween-target-values self) i)
                  (if (tween-initializedp self)
                      (+ (aref (tween-start-values self) i) arg) arg))))

(declaim (ftype (function (tween &rest f32)) tween-add-waypoint))
(defun tween-add-waypoint (self &rest args)
  (loop :initially (when (= (tween-waypoints-count self) +tween-waypoints-limit+)
                     (error "Waypoint limit is exceeded."))
        :with num-args :of-type i32 := (length args)
        :for arg :of-type f32 :in args
        :for i :of-type i32 :from 0
        :do (setf (aref (tween-waypoints self) (+ (* num-args (tween-waypoints-count self)) i)) arg)
        :finally (incf (tween-waypoints-count self))))

(declaim (ftype (function (tween)) tween-build))
(defun tween-build (self)
  (funcall (tween-values-getter self) (tween-accessor-buffer self)))

(defmethod base-tween-build ((self tween))
  (tween-build self))

(declaim (ftype (function (tween)) tween-free))
(defun tween-free (self)
  (pool-free *tween-pool* self))

(defmethod base-tween-free ((self tween))
  (tween-free self))

(declaim (ftype (function (tween)) tween-initialize-override))
(defun tween-initialize-override (self)
  (loop :initially (funcall (tween-values-getter self) (tween-start-values self))
        :for i :of-type i32 :below (tween-combined-attributes-count self)
        :do (loop :initially (when (tween-relativep self)
                               (incf (aref (tween-target-values self) i) (aref (tween-start-values self) i)))
                  :for j :of-type i32 :below (tween-waypoints-count self)
                  :do (when (tween-relativep self)
                        (incf (aref (tween-waypoints self) (+ (* j (tween-combined-attributes-count self)) i))
                              (aref (tween-start-values self) i)))
                  :finally (when (tween-fromp self)
                             (rotatef (aref (tween-start-values self) i) (aref (tween-target-values self) i))))))

(defmethod base-tween-initialize-override ((self tween))
  (tween-initialize-override self))

(declaim (ftype (function (tween i32 i32 boolean f32)) tween-update-override))
(defun tween-update-override (self step last-step iteration-step-p delta)
  (unless iteration-step-p
    (cond
      ((> step last-step)
       (funcall (tween-values-setter self)
                (if (base-tween-reversep self last-step)
                    (tween-start-values self)
                    (tween-target-values self)))
       (return-from tween-update-override))
      ((< step last-step)
       (funcall (tween-values-setter self)
                (if (base-tween-reversep self last-step)
                    (tween-target-values self)
                    (tween-start-values self)))
       (return-from tween-update-override))))
  (assert iteration-step-p)
  (assert (<= 0.0 (tween-current-time self) (tween-duration self)))
  (when (< (tween-duration self) 0.00000000001)
    (cond
      ((> delta -0.00000000001)
       (funcall (tween-values-setter self)
                (if (base-tween-reversep self last-step)
                    (tween-target-values self)
                    (tween-start-values self)))
       (return-from tween-update-override))
      ((< delta 0.00000000001)
       (funcall (tween-values-setter self)
                (if (base-tween-reversep self last-step)
                    (tween-start-values self)
                    (tween-target-values self)))
       (return-from tween-update-override))))
  (let* ((time (if (base-tween-reversep self step)
                   (- (tween-duration self) (tween-current-time self))
                   (tween-current-time self)))
         (x (funcall (tween-equation self) (/ time (tween-duration self)))))
    (if (zerop (tween-waypoints-count self))
        (loop :for i :of-type i32 :below (tween-combined-attributes-count self)
              :do (setf (aref (tween-accessor-buffer self) i)
                        (+ (aref (tween-start-values self) i)
                           (* x (- (aref (tween-target-values self) i)
                                   (aref (tween-start-values self) i))))))
        (loop :for i :of-type i32 :below (tween-combined-attributes-count self)
              :do (loop :initially (setf (aref (tween-path-buffer self) 0)
                                         (aref (tween-start-values self) i)
                                         (aref (tween-path-buffer self) (1+ (tween-waypoints-count self)))
                                         (aref (tween-target-values self) i))
                        :for j :of-type i32 :below (tween-waypoints-count self)
                        :do (setf (aref (tween-path-buffer self) (1+ j))
                                  (aref (tween-waypoints self) (+ i (* j (tween-combined-attributes-count self)))))
                        :finally (setf (aref (tween-accessor-buffer self) i)
                                       (funcall (tween-path self) x (tween-path-buffer self) (+ (tween-waypoints-count self) 2))))))
    (funcall (tween-values-setter self) (tween-accessor-buffer self))))

(defmethod base-tween-update-override ((self tween) step last-step iteration-step-p delta)
  (tween-update-override self step last-step iteration-step-p delta))

(declaim (ftype (function (tween)) tween-force-start-values))
(defun tween-force-start-values (self)
  (funcall (tween-values-setter self) (tween-start-values self)))

(defmethod base-tween-force-start-values ((self tween))
  (tween-force-start-values self))

(declaim (ftype (function (tween)) tween-force-end-values))
(defun tween-force-end-values (self)
  (funcall (tween-values-setter self) (tween-target-values self)))

(defmethod base-tween-force-end-values ((self tween))
  (tween-force-end-values self))
