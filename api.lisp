(in-package #:universal-tween-engine)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf (fdefinition 'killedp) (fdefinition 'base-tween-killedp)
        (fdefinition 'pausedp) (fdefinition 'base-tween-pausedp)
        (fdefinition 'startedp) (fdefinition 'base-tween-startedp)
        (fdefinition 'finishedp) (fdefinition 'base-tween-finishedp)
        (fdefinition 'duration) (fdefinition 'base-tween-duration)
        (fdefinition 'full-duration) (fdefinition 'base-tween-full-duration)
        (fdefinition 'add-child) (fdefinition 'timeline-add-child)
        (fdefinition 'add-waypoint) (fdefinition 'tween-add-waypoint)))

(declaim (ftype (function (base-tween) (values base-tween)) build)
         (inline build))
(defun build (tween)
  (base-tween-build tween) tween)

(declaim (ftype (function (base-tween &optional tween-manager) (values base-tween)) start)
         (inline start))
(defun start (tween &optional (manager *tween-manager*))
  (tween-manager-add manager tween) tween)

(declaim (ftype (function (base-tween) (values base-tween)) stop)
         (inline kill))
(defun kill (tween)
  (base-tween-kill tween) tween)

(declaim (ftype (function (base-tween) (values base-tween)) resume)
         (inline resume))
(defun resume (tween)
  (base-tween-resume tween) tween)

(declaim (ftype (function (base-tween) (values base-tween)) pause)
         (inline pause))
(defun pause (tween)
  (base-tween-pause tween) tween)

(declaim (ftype (function (&optional tween-manager)) pause-all)
         (inline pause-all))
(defun pause-all (&optional (manager *tween-manager*))
  (tween-manager-pause manager))

(declaim (ftype (function (&optional tween-manager)) resume-all)
         (inline resume-all))
(defun resume-all (&optional (manager *tween-manager*))
  (tween-manager-resume manager))

(declaim (ftype (function (&optional tween-manager)) kill-all)
         (inline kill-all))
(defun kill-all (&optional (manager *tween-manager*))
  (tween-manager-kill-all manager))

(declaim (ftype (function (f32 &optional tween-manager)) update)
         (inline update))
(defun update (delta &optional (manager *tween-manager*))
  (tween-manager-update manager delta))

(define-constant +tween-equations+ '(tween-equation-back-in
                                     tween-equation-back-out
                                     tween-equation-back-inout
                                     tween-equation-bounce-out
                                     tween-equation-bounce-in
                                     tween-equation-bounce-inout
                                     tween-equation-circ-in
                                     tween-equation-circ-out
                                     tween-equation-circ-inout
                                     tween-equation-cubic-in
                                     tween-equation-cubic-out
                                     tween-equation-cubic-inout
                                     tween-equation-elastic-in
                                     tween-equation-elastic-out
                                     tween-equation-elastic-inout
                                     tween-equation-expo-in
                                     tween-equation-expo-out
                                     tween-equation-expo-inout
                                     tween-equation-quad-in
                                     tween-equation-quad-out
                                     tween-equation-quad-inout
                                     tween-equation-quart-in
                                     tween-equation-quart-out
                                     tween-equation-quart-inout
                                     tween-equation-quint-in
                                     tween-equation-quint-out
                                     tween-equation-quint-inout
                                     tween-equation-sine-in
                                     tween-equation-sine-out
                                     tween-equation-linear-inout)
  :test #'equal)

(define-constant +tween-paths+ '(tween-path-catmull-rom tween-path-linear)
  :test #'equal)

(define-constant +easings+ (loop :for symbol :in +tween-equations+
                                 :collect (intern (subseq (symbol-name symbol) (length (string '#:tween-equation-)))))
  :test #'equal)

(define-constant +paths+ (loop :for symbol :in +tween-paths+
                               :collect (intern (subseq (symbol-name symbol) (length (string '#:tween-path-)))))
  :test #'equal)

(loop :for symbol :in +easings+
      :for definition :in +tween-equations+
      :do (setf (fdefinition symbol) (fdefinition definition)))

(loop :for symbol :in +paths+
      :for definition :in +tween-paths+
      :do (setf (fdefinition symbol) (fdefinition definition)))

(deftype callback () 'tween-callback)

(defun callback (tween)
  (base-tween-callback tween))

(defun (setf callback) (value tween)
  (setf (base-tween-callback tween) value))

(defconstant +callback-none+ +tween-callback-none+)
(defconstant +callback-begin+ +tween-callback-begin+)
(defconstant +callback-start+ +tween-callback-start+)
(defconstant +callback-end+ +tween-callback-end+)
(defconstant +callback-complete+ +tween-callback-complete+)
(defconstant +callback-back-begin+ +tween-callback-back-begin+)
(defconstant +callback-back-start+ +tween-callback-back-start+)
(defconstant +callback-back-end+ +tween-callback-back-end+)
(defconstant +callback-back-complete+ +tween-callback-back-complete+)
(defconstant +callback-any-forward+ +tween-callback-any-forward+)
(defconstant +callback-any-backward+ +tween-callback-any-backward+)
(defconstant +callback-any+ +tween-callback-any+)

(defun callback-type () *tween-callback-type*)

(defmacro tween (&key
                   (from nil)
                   (to nil)
                   (path '#'tween-path-catmull-rom)
                   (ease ''tween-equation-quad-inout)
                   (delay 0.0)
                   (repeat 0)
                   (relativep nil)
                   (duration 0.0)
                   (callback '#'values)
                 &aux
                   (pwt (or from to))
                   (places (car pwt))
                   (waypoints (butlast (cdr pwt)))
                   (targets (lastcar (cdr pwt))))
  (let ((n-places (length places))
        (n-targets (length targets)))
    (assert (= n-places n-targets) nil "The number of places must equal to the number of targets.")
    (with-gensyms (tween values)
      `(let ((,tween (pool-get *tween-pool*)))
         (tween-setup ,tween ,n-places
                      (lambda (,values)
                        (declare
                         (ignorable ,values)
                         (type (simple-array single-float (*)) ,values))
                        ,@(loop :for place :in places
                                :for i :of-type u8 :from 0
                                :collect `(setf (aref ,values ,i) ,place)))
                      (lambda (,values)
                        (declare
                         (ignorable ,values)
                         (type (simple-array single-float (*)) ,values))
                        ,@(loop :for place :in places
                                :for i :of-type u8 :from 0
                                :collect `(setf ,place (aref ,values ,i))))
                      ,duration)
         ,(destructuring-bind (&key (function '#'values) (triggers '+callback-complete+))
              (if (and (listp callback) (keywordp (car callback))) callback
                  (list :function callback))
            `(setf (tween-path ,tween) ,path
                   (tween-equation ,tween) (ensure-function ,ease)
                   (tween-fromp ,tween) ,(not (null from))
                   (tween-callback ,tween) (or ,function #'values) 
                   (tween-delay ,tween) ,delay
                   (base-tween-callback-triggers ,tween) ,triggers))
         ,(destructuring-bind (&key (count 0) (delay 0.0) (yoyop nil))
              (if (and (listp repeat) (keywordp (car repeat))) repeat
                  (list :count repeat))
            `(base-tween-set-repeat ,tween ,(with-gensyms (var)
                                              `(let ((,var ,count))
                                                 (etypecase ,var
                                                   (boolean (if ,var +tween-repeat-infinity+ 0))
                                                   (i32 ,var))))
                                    ,delay ,yoyop))
         (progn . ,(loop :for waypoint :in waypoints
                         :collect `(tween-add-waypoint ,tween . ,waypoint)))
         ,(if relativep
              `(tween-set-relative-target ,tween . ,targets)
              `(tween-set-target ,tween . ,targets))
         ,tween))))

(defmacro timeline (desc &rest args)
  (with-gensyms (timeline)
    (destructuring-ecase desc
      (((:tween :timeline) &rest descs)
       (cond
         ((cdr descs) `(,(find-symbol (symbol-name (car desc)) (symbol-package 'timeline)) ,@descs ,@args))
         ((listp (car descs)) `(,@(car descs) ,@args))
         (t (car descs))))
      ((:pause time)
       `(tween :delay ,time))
      ((:call function &rest args)
       (if-let ((repeat-options (getf args :repeat)))
         `(timeline (:sequence (:call ,function . ,(remove-from-plist args :repeat)) :repeat ,repeat-options))
         `(tween :callback ,function . ,args)))
      (((:from :to) &rest args)
       (declare (ignore args))
       `(tween . ,desc))
      (((:sequence :parallel) &rest descs)
       `(let ((,timeline (pool-get *timeline-pool*)))
          (timeline-setup ,timeline ,(car desc))
          ,@(loop :for timeline-args :on descs
                  :for desc :in descs
                  :if (keywordp desc)
                    :do (setf args timeline-args)
                    :and :return add-children
                  :else
                    :collect `(timeline-add-child ,timeline (timeline ,desc)) :into add-children
                  :finally (return add-children))
          ,@(destructuring-bind (&key (repeat 0) (delay 0.0) (callback '#'values)) args
              `((base-tween-set-repeat ,timeline .
                                       ,(destructuring-bind (&key (count 0) (delay 0.0) (yoyop nil))
                                            (if (and (listp repeat) (keywordp (car repeat))) repeat
                                                (list :count repeat))
                                          `(,(with-gensyms (var)
                                               `(let ((,var ,count))
                                                  (etypecase ,var
                                                    (boolean (if ,var +tween-repeat-infinity+ 0))
                                                    (i32 ,var))))
                                            ,delay ,yoyop)))
                ,(destructuring-bind (&key (function '#'values) (triggers '+callback-complete+))
                     (if (and (listp callback) (keywordp (car callback))) callback
                         (list :function callback))
                   `(setf (timeline-delay ,timeline) ,delay
                          (timeline-callback ,timeline) (or ,function #'values)
                          (timeline-callback-triggers ,timeline) ,triggers))))
          ,timeline)))))
