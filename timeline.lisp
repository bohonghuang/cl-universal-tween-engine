(in-package #:universal-tween-engine)

(deftype timeline-mode () `(member :sequence :parallel))

(defstruct (timeline (:include base-tween))
  (children (make-array 0 :element-type 'base-tween
                          :fill-pointer 0
                          :adjustable t)
   :type (vector base-tween))
  (builtp nil :type boolean)
  (mode :sequence :type timeline-mode))

(declaim (ftype (function (timeline)) timeline-reset))
(defun timeline-reset (self)
  (%base-tween-reset self)
  (setf (fill-pointer (timeline-children self)) 0
        (timeline-builtp self) nil))

(defmethod base-tween-reset ((self timeline))
  (timeline-reset self))

(defun make-timeline-pool ()
  (make-pool :object-creator #'make-timeline
             :callback (make-pool-callback
                        :on-pool (lambda (timeline)
                                   (timeline-reset timeline)
                                   (setf (timeline-finishedp timeline) t
                                         (timeline-killedp timeline) t))
                        :on-unpool (lambda (timeline)
                                     (assert (emptyp (timeline-children timeline)))
                                     (assert (eq (timeline-callback timeline) #'values))
                                     (assert (timeline-auto-remove-enabled-p timeline))
                                     (setf (timeline-finishedp timeline) nil
                                           (timeline-killedp timeline) nil)))))

(defparameter *timeline-pool* (make-timeline-pool))

(defmacro do-timeline-children (((var self) &key from-end) &body body)
  (with-gensyms (i)
    `(locally #+sbcl (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
              (loop :for ,i :of-type i32 ,@(if from-end
                                               `(:downfrom (1- (length (timeline-children ,self))) :to 0)
                                               `(:below (length (timeline-children ,self))))
                    :for ,var :of-type base-tween := (aref (timeline-children ,self) ,i)
                    :do (locally #+sbcl (declare (sb-ext:unmuffle-conditions sb-ext:compiler-note)) ,@body)))))

(declaim (ftype (function (timeline)) timeline-build))
(defun timeline-build (self)
  (when (timeline-builtp self)
    (return-from timeline-build))
  (setf (timeline-duration self) 0.0)
  (do-timeline-children ((object self))
    (when (minusp (base-tween-repeat-count object))
      (error "You can't push an object with infinite repetitions in a timeline."))
    (base-tween-build object)
    (case (timeline-mode self)
      (:sequence (let ((tdelay (timeline-duration self)))
                   (incf (timeline-duration self) (base-tween-full-duration object))
                   (incf (base-tween-delay object) tdelay)))
      (:parallel (setf (timeline-duration self) (max (timeline-duration self)
                                                     (base-tween-full-duration object))))))
  (setf (timeline-builtp self) t))

(defmethod base-tween-build ((self timeline))
  (timeline-build self))

(declaim (ftype (function (timeline)) timeline-start))
(defun timeline-start (self)
  (%base-tween-start self)
  (do-timeline-children ((object self))
    (base-tween-start object)))

(defmethod base-tween-start ((self timeline))
  (timeline-start self))

(declaim (ftype (function (timeline)) timeline-free))
(defun timeline-free (self)
  (do-timeline-children ((object self))
    (base-tween-free object))
  (pool-free *timeline-pool* self))

(defmethod base-tween-free ((self timeline))
  (timeline-free self))

(declaim (ftype (function (timeline)) timeline-force-start-values))
(defun timeline-force-start-values (self)
  (do-timeline-children ((object self) :from-end t)
    (base-tween-force-to-start object)))

(defmethod base-tween-force-start-values ((self timeline))
  (timeline-force-start-values self))

(declaim (ftype (function (timeline)) timeline-force-end-values))
(defun timeline-force-end-values (self)
  (do-timeline-children ((object self) :from-end nil)
    (base-tween-force-to-end object (timeline-duration self))))

(defmethod base-tween-force-end-values ((self timeline))
  (timeline-force-end-values self))

(defmethod base-tween-initialize-override ((self timeline)) (declare (ignore self)))

(declaim (ftype (function (base-tween i32 i32 boolean f32)) timeline-update-override))
(defun timeline-update-override (self step last-step iteration-step-p delta)
  (unless iteration-step-p
    (let ((dt (if (base-tween-reversep self last-step) (- delta) (+ delta))))
      (declare (type f32 dt))
      (incf dt (float-sign dt 1.0))
      (cond
        ((> step last-step)
         (assert (not (minusp delta)))
         (do-timeline-children ((object self) :from-end nil)
           (base-tween-update object dt))
         (return-from timeline-update-override))
        ((< step last-step)
         (assert (not (plusp delta)))
         (do-timeline-children ((object self) :from-end t)
           (base-tween-update object dt))
         (return-from timeline-update-override)))))
  (assert iteration-step-p)
  (cond
    ((> step last-step)
     (if (base-tween-reversep self step)
         (progn
           (timeline-force-end-values self)
           (do-timeline-children ((object self) :from-end nil)
             (base-tween-update object delta)))
         (progn
           (timeline-force-start-values self)
           (do-timeline-children ((object self) :from-end nil)
             (base-tween-update object delta)))))
    ((< step last-step)
     (if (base-tween-reversep self step)
         (progn
           (timeline-force-start-values self)
           (do-timeline-children ((object self) :from-end t)
             (base-tween-update object delta)))
         (progn
           (timeline-force-end-values self)
           (do-timeline-children ((object self) :from-end t)
             (base-tween-update object delta)))))
    (t (let ((dt (if (base-tween-reversep self step) (- delta) (+ delta))))
         (declare (type f32 dt))
         (if (minusp delta)
             (do-timeline-children ((object self) :from-end t)
               (base-tween-update object dt))
             (do-timeline-children ((object self) :from-end nil)
               (base-tween-update object dt)))))))

(defmethod base-tween-update-override ((self timeline) step last-step iteration-step-p delta)
  (timeline-update-override self step last-step iteration-step-p delta))

(declaim (ftype (function (timeline base-tween)) timeline-add-child))
(defun timeline-add-child (self object)
  (vector-push-extend object (timeline-children self)))

(declaim (ftype (function (timeline timeline-mode)) timeline-setup))
(defun timeline-setup (self mode)
  (setf (timeline-mode self) mode))
