(in-package #:universal-tween-engine)

(defstruct (base-tween (:constructor nil))
  (step 0 :type i32)
  (repeat-count 0 :type i32)
  (iteration-step-p nil :type boolean)
  (yoyop nil :type boolean)
  (delay 0.0 :type f32)
  (duration 0.0 :type f32)
  (repeat-delay 0.0 :type f32)
  (current-time 0.0 :type f32)
  (delta-time 0.0 :type f32)
  (startedp nil :type boolean)
  (initializedp nil :type boolean)
  (finishedp nil :type boolean)
  (killedp nil :type boolean)
  (pausedp nil :type boolean)
  (callback #'values :type tween-callback)
  (callback-triggers +tween-callback-complete+ :type u8)
  (auto-remove-enabled-p t :type boolean)
  (auto-start-enabled-p t :type boolean))

(declaim (ftype (function (base-tween)) %base-tween-reset))
(defun %base-tween-reset (self)
  (setf (base-tween-step self) -2
        (base-tween-repeat-count self) 0
        (base-tween-iteration-step-p self) nil
        (base-tween-yoyop self) nil
        (base-tween-delay self) 0.0
        (base-tween-duration self) 0.0
        (base-tween-repeat-delay self) 0.0
        (base-tween-current-time self) 0.0
        (base-tween-delta-time self) 0.0
        (base-tween-startedp self) nil
        (base-tween-initializedp self) nil
        (base-tween-finishedp self) nil
        (base-tween-killedp self) nil
        (base-tween-pausedp self) nil
        (base-tween-callback self) #'values
        (base-tween-callback-triggers self) +tween-callback-complete+ 
        (base-tween-auto-remove-enabled-p self) t
        (base-tween-auto-start-enabled-p self) t))

(defgeneric base-tween-reset (self))

(defgeneric base-tween-build (self))

(declaim (ftype (function (base-tween)) %base-tween-start))
(defun %base-tween-start (self)
  (base-tween-build self)
  (setf (base-tween-current-time self) 0.0
        (base-tween-startedp self) t))

(defgeneric base-tween-start (self))

(declaim (ftype (function (base-tween)) base-tween-kill))
(defun base-tween-kill (self)
  (setf (base-tween-killedp self) t))

(defgeneric base-tween-free (self))

(declaim (ftype (function (base-tween)) base-tween-pause))
(defun base-tween-pause (self)
  (setf (base-tween-pausedp self) t))

(declaim (ftype (function (base-tween)) base-tween-resume))
(defun base-tween-resume (self)
  (setf (base-tween-pausedp self) nil))

(declaim (ftype (function (base-tween i32 f32 &optional boolean)) base-tween-set-repeat))
(defun base-tween-set-repeat (self count delay &optional yoyop)
  (when (base-tween-startedp self)
    (error "You can't change the repetitions of a tween or timeline once it is started."))
  (setf (base-tween-repeat-count self) count
        (base-tween-repeat-delay self) (max delay 0.0)
        (base-tween-yoyop self) yoyop))

(declaim (ftype (function (base-tween) (values f32)) base-tween-full-duration))
(defun base-tween-full-duration (self)
  (if (minusp (base-tween-repeat-count self)) -1
      (+ (base-tween-delay self)
         (base-tween-duration self)
         (* (+ (base-tween-repeat-delay self)
               (base-tween-duration self))
            (base-tween-repeat-count self)))))

(defgeneric base-tween-force-start-values (self))

(defgeneric base-tween-force-end-values (self))

(defgeneric base-tween-initialize-override (self))

(defgeneric base-tween-update-override (self step last-step iteration-step-p delta))

(declaim (ftype (function (base-tween &optional i32) (values boolean)) base-tween-reversep))
(defun base-tween-reversep (self &optional (step (base-tween-step self)))
  (and (base-tween-yoyop self) (= (abs (rem step 4)) 2)))

(declaim (ftype (function (base-tween &optional i32) (values boolean)) base-tween-validp))
(defun base-tween-validp (self &optional (step (base-tween-step self)))
  (or (<= 0 step (* (base-tween-repeat-count self) 2))
      (minusp (base-tween-repeat-count self))))

(declaim (ftype (function (base-tween)) base-tween-force-to-start))
(defun base-tween-force-to-start (self)
  (setf (base-tween-current-time self) (- (base-tween-delay self))
        (base-tween-step self) -1
        (base-tween-iteration-step-p self) nil)
  (if (base-tween-reversep self 0)
      (base-tween-force-end-values self)
      (base-tween-force-start-values self)))

(declaim (ftype (function (base-tween f32)) base-tween-force-to-end))
(defun base-tween-force-to-end (self time)
  (setf (base-tween-current-time self) (- time (base-tween-full-duration self))
        (base-tween-step self) (1+ (* (base-tween-repeat-count self) 2))
        (base-tween-iteration-step-p self) nil)
  (if (base-tween-reversep self (* (base-tween-repeat-count self) 2))
      (base-tween-force-start-values self)
      (base-tween-force-end-values self)))

(declaim (ftype (function (base-tween u8)) base-tween-call-callback))
(defun base-tween-call-callback (self type)
  (when (plusp (logand (base-tween-callback-triggers self) type))
    (let ((*tween-callback-type* type))
      (funcall (base-tween-callback self)))))

(declaim (ftype (function (base-tween)) base-tween-initialize))
(defun base-tween-initialize (self)
  (when (>= (+ (base-tween-current-time self) (base-tween-delta-time self))
            (base-tween-delay self))
    (base-tween-initialize-override self)
    (setf (base-tween-initializedp self) t
          (base-tween-iteration-step-p self) t
          (base-tween-step self) 0)
    (decf (base-tween-delta-time self) (- (base-tween-delay self)
                                          (base-tween-current-time self)))
    (setf (base-tween-current-time self) 0.0)
    (base-tween-call-callback self +tween-callback-begin+)
    (base-tween-call-callback self +tween-callback-start+)))

(declaim (ftype (function (base-tween)) base-tween-test-relaunch))
(defun base-tween-test-relaunch (self)
  (cond
    ((and (not (base-tween-iteration-step-p self))
          (not (minusp (base-tween-repeat-count self)))
          (minusp (base-tween-step self))
          (not (minusp (+ (base-tween-current-time self)
                          (base-tween-delta-time self)))))
     (assert (= (base-tween-step self) -1))
     (setf (base-tween-iteration-step-p self) t
           (base-tween-step self) 0)
     (let ((delta (- 0.0 (base-tween-current-time self))))
       (decf (base-tween-delta-time self) delta)
       (setf (base-tween-current-time self) 0.0)
       (base-tween-call-callback self +tween-callback-begin+)
       (base-tween-call-callback self +tween-callback-start+)
       (base-tween-update-override self
                                   (base-tween-step self)
                                   (1- (base-tween-step self))
                                   (base-tween-iteration-step-p self)
                                   delta)))
    ((and (not (base-tween-iteration-step-p self))
          (not (minusp (base-tween-repeat-count self)))
          (> (base-tween-step self) (* (base-tween-repeat-count self) 2))
          (minusp (+ (base-tween-current-time self)
                     (base-tween-delta-time self))))
     (assert (= (base-tween-step self) (1+ (* (base-tween-repeat-count self) 2))))
     (setf (base-tween-iteration-step-p self) t
           (base-tween-step self) (* (base-tween-repeat-count self) 2))
     (let ((delta (- 0.0 (base-tween-current-time self))))
       (decf (base-tween-delta-time self) delta)
       (setf (base-tween-current-time self) (base-tween-duration self))
       (base-tween-call-callback self +tween-callback-back-begin+)
       (base-tween-call-callback self +tween-callback-back-start+)
       (base-tween-update-override self
                                   (base-tween-step self)
                                   (1+ (base-tween-step self))
                                   (base-tween-iteration-step-p self)
                                   delta)))))

(declaim (ftype (function (base-tween)) base-tween-update-step))
(defun base-tween-update-step (self)
  (loop :while (base-tween-validp self)
        :do (cond
              ((and (not (base-tween-iteration-step-p self))
                    (>= 0 (+ (base-tween-current-time self)
                             (base-tween-delta-time self))))
               (setf (base-tween-iteration-step-p self) t)
               (decf (base-tween-step self))
               (let ((delta (- 0.0 (base-tween-current-time self))))
                 (decf (base-tween-delta-time self) delta)
                 (setf (base-tween-current-time self) (base-tween-duration self))
                 (if (base-tween-reversep self)
                     (base-tween-force-start-values self)
                     (base-tween-force-end-values self))
                 (base-tween-call-callback self +tween-callback-back-start+)
                 (base-tween-update-override self
                                             (base-tween-step self)
                                             (1+ (base-tween-step self))
                                             (base-tween-iteration-step-p self)
                                             delta)))
              ((and (not (base-tween-iteration-step-p self))
                    (<= (base-tween-repeat-delay self)
                        (+ (base-tween-current-time self)
                           (base-tween-delta-time self))))
               (setf (base-tween-iteration-step-p self) t)
               (incf (base-tween-step self))
               (let ((delta (- (base-tween-repeat-delay self)
                               (base-tween-current-time self))))
                 (decf (base-tween-delta-time self) delta)
                 (setf (base-tween-current-time self) 0.0)
                 (if (base-tween-reversep self)
                     (base-tween-force-end-values self)
                     (base-tween-force-start-values self))
                 (base-tween-call-callback self +tween-callback-start+)
                 (base-tween-update-override self
                                             (base-tween-step self)
                                             (1- (base-tween-step self))
                                             (base-tween-iteration-step-p self)
                                             delta)))
              ((and (base-tween-iteration-step-p self)
                    (> 0 (+ (base-tween-current-time self)
                            (base-tween-delta-time self))))
               (setf (base-tween-iteration-step-p self) nil)
               (decf (base-tween-step self))
               (let ((delta (- 0.0 (base-tween-current-time self))))
                 (decf (base-tween-delta-time self) delta)
                 (setf (base-tween-current-time self) 0.0)
                 (base-tween-update-override self
                                             (base-tween-step self)
                                             (1+ (base-tween-step self))
                                             (base-tween-iteration-step-p self)
                                             delta)
                 (base-tween-call-callback self +tween-callback-back-end+)
                 (if (and (< (base-tween-step self) 0)
                          (not (minusp (base-tween-repeat-count self))))
                     (base-tween-call-callback self +tween-callback-complete+)
                     (setf (base-tween-current-time self) (base-tween-repeat-delay self)))))
              ((and (base-tween-iteration-step-p self)
                    (< (base-tween-duration self)
                       (+ (base-tween-current-time self)
                          (base-tween-delta-time self))))
               (setf (base-tween-iteration-step-p self) nil)
               (incf (base-tween-step self))
               (let ((delta (- (base-tween-duration self)
                               (base-tween-current-time self))))
                 (decf (base-tween-delta-time self) delta)
                 (setf (base-tween-current-time self) (base-tween-duration self))
                 (base-tween-update-override self
                                             (base-tween-step self)
                                             (1- (base-tween-step self))
                                             (base-tween-iteration-step-p self)
                                             delta)
                 (base-tween-call-callback self +tween-callback-end+)
                 (when (and (> (base-tween-step self) (* (base-tween-repeat-count self) 2))
                            (not (minusp (base-tween-repeat-count self))))
                   (base-tween-call-callback self +tween-callback-complete+))
                 (setf (base-tween-current-time self) 0.0)))
              ((base-tween-iteration-step-p self)
               (let ((delta (base-tween-delta-time self)))
                 (decf (base-tween-delta-time self) delta)
                 (incf (base-tween-current-time self) delta)
                 (base-tween-update-override self
                                             (base-tween-step self)
                                             (base-tween-step self)
                                             (base-tween-iteration-step-p self)
                                             delta)
                 (return)))
              (t (let ((delta (base-tween-delta-time self)))
                   (decf (base-tween-delta-time self) delta)
                   (incf (base-tween-current-time self) delta)
                   (return))))))

(declaim (ftype (function (base-tween)) base-tween-test-completion))
(defun base-tween-test-completion (self)
  (setf (base-tween-finishedp self) (and (not (minusp (base-tween-repeat-count self)))
                                         (or (> (base-tween-step self)
                                                (* (base-tween-repeat-count self) 2))
                                             (minusp (base-tween-step self))))))

(declaim (ftype (function (base-tween f32)) base-tween-update))
(defun base-tween-update (self delta)
  (when (or (not (base-tween-startedp self))
            (base-tween-pausedp self)
            (base-tween-killedp self))
    (return-from base-tween-update))
  (setf (base-tween-delta-time self) delta)
  (unless (base-tween-initializedp self)
    (base-tween-initialize self))
  (when (base-tween-initializedp self)
    (base-tween-test-relaunch self)
    (base-tween-update-step self)
    (base-tween-test-completion self))
  (incf (base-tween-current-time self) (base-tween-delta-time self))
  (setf (base-tween-delta-time self) 0.0))
