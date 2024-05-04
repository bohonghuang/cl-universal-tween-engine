(in-package #:universal-tween-engine)

(defstruct pool-callback
  (on-pool #'values :type (function (t)))
  (on-unpool #'values :type (function (t))))

(defstruct pool
  (objects (make-array 8 :element-type t
                         :fill-pointer 0
                         :adjustable t
                         :initial-element nil)
   :type (vector base-tween))
  (object-creator nil :type (function () base-tween))
  (callback (make-pool-callback) :type pool-callback))

(declaim (ftype (function (pool) t) pool-create))
(defun pool-create (self)
  (funcall (pool-object-creator self)))

(declaim (ftype (function (pool) t) pool-get))
(defun pool-get (self)
  (let ((obj (if (emptyp (pool-objects self))
                 (pool-create self)
                 (vector-pop (pool-objects self)))))
    (funcall (pool-callback-on-unpool (pool-callback self)) obj)
    (values obj)))

(declaim (ftype (function (pool t)) pool-free))
(defun pool-free (self obj)
  (assert (null (find obj (pool-objects self))))
  (funcall (pool-callback-on-pool (pool-callback self)) obj)
  (vector-push-extend obj (pool-objects self)))

(declaim (ftype (function (pool)) pool-clear))
(defun pool-clear (self)
  (setf (fill-pointer (pool-objects self)) 0))

(declaim (ftype (function (pool) i32) pool-size))
(defun pool-size (self)
  (length (pool-objects self)))

(declaim (ftype (function (pool i32)) pool-ensure-capacity))
(defun pool-ensure-capacity (self capacity)
  (setf (pool-objects self) (adjust-array (pool-objects self) capacity)))
