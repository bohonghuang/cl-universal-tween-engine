(in-package #:universal-tween-engine)

(defstruct tween-manager
  (objects (make-array 0 :element-type 'base-tween
                         :fill-pointer 0
                         :adjustable t)
   :type (array base-tween (*)))
  (pausedp nil :type boolean))

(defparameter *tween-manager* (make-tween-manager))

(declaim (ftype (function (tween-manager base-tween)) tween-manager-add))
(defun tween-manager-add (self object)
  (let ((objects (tween-manager-objects self)))
    (assert (null (find object objects)))
    (vector-push-extend object objects))
  (when (base-tween-auto-start-enabled-p object)
    (base-tween-start object)))

(declaim (ftype (function (tween-manager)) tween-manager-kill-all))
(defun tween-manager-kill-all (self)
  (loop :for object :of-type base-tween :across (tween-manager-objects self)
        :do (base-tween-kill object)))

(declaim (ftype (function (tween-manager i32) *) tween-manager-ensure-capacity))
(defun tween-manager-ensure-capacity (self capacity)
  (setf (tween-manager-objects self) (adjust-array (tween-manager-objects self) capacity)))

(declaim (ftype (function (tween-manager)) tween-manager-pause))
(defun tween-manager-pause (self)
  (setf (tween-manager-pausedp self) t))

(declaim (ftype (function (tween-manager)) tween-manager-resume))
(defun tween-manager-resume (self)
  (setf (tween-manager-pausedp self) nil))

(declaim (ftype (function (base-tween) (values boolean)) tween-manager-object-deletable-p))
(defun tween-manager-object-deletable-p (object)
  (let ((finishedp (base-tween-finishedp object))
        (killedp (base-tween-killedp object)))
    (when (and (or finishedp killedp) (base-tween-auto-remove-enabled-p object))
      (when (xor finishedp killedp)
        (base-tween-free object))
      t)))

(declaim (ftype (function (tween-manager f32)) tween-manager-update))
(defun tween-manager-update (self delta)
  (let ((objects (setf (tween-manager-objects self) (delete-if #'tween-manager-object-deletable-p (tween-manager-objects self)))))
    (unless (tween-manager-pausedp self)
      (if (minusp delta)
          (loop :for i :downfrom (1- (length objects)) :to 0
                :do (base-tween-update (aref objects i) delta))
          (loop :for i :below (length objects)
                :do (base-tween-update (aref objects i) delta))))))

(declaim (ftype (function ((vector base-tween)) i32) count-tweens))
(defun count-tweens (objects)
  (loop :for object :across objects
        :summing (etypecase object
                   (tween 1)
                   (timeline (count-tweens (timeline-children object))))))

(declaim (ftype (function ((vector base-tween)) i32) count-timelines))
(defun count-timelines (objects)
  (loop :for object :across objects
        :summing (etypecase object
                   (tween 0)
                   (timeline (1+ (count-timelines (timeline-children object)))))))

(declaim (ftype (function (tween-manager) i32) tween-manager-running-tweens-count))
(defun tween-manager-running-tweens-count (self)
  (count-tweens (tween-manager-objects self)))

(declaim (ftype (function (tween-manager) i32) tween-manager-running-timelines-count))
(defun tween-manager-running-timelines-count (self)
  (count-timelines (tween-manager-objects self)))
