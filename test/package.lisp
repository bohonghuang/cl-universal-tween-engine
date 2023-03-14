(defpackage universal-tween-engine-test
  (:use #:cl #:parachute)
  (:nicknames #:ute.test))

(in-package #:universal-tween-engine-test)

(define-test suite)

(defun system-absolute-pathname (pathname)
  (merge-pathnames pathname (asdf:component-pathname (asdf:find-system '#:universal-tween-engine/test))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf (fdefinition 'read-from-file) (alexandria:compose #'read-from-string #'alexandria:read-file-into-string)))

(defun float-equal (f1 f2)
  (< (abs (- f1 f2)) 1e-5))

(defun float-list-equal (l1 l2)
  (every #'float-equal l1 l2))

(define-test timeline :parent suite
  (loop :with ute:*tween-manager* := (ute:make-tween-manager)
        :with delta := 0.1
        :with f1 := 0.0 :and f2 := 0.0
        :with timeline := (ute:start
                           (ute:timeline
                            (:sequence
                             (:parallel
                              (:to ((f1) (10.0)) :ease #'ute:quad-inout :duration 2.5)
                              (:to ((f2) (50.0)) :ease #'ute:quad-inout :duration 1.5))
                             (:parallel
                              (:to ((f1) (20.0) (-10.0)) :ease #'ute:quad-inout :path #'ute:catmull-rom :duration 2.0)
                              (:tween :to ((f2) (-20.0)) :ease #'ute:quad-inout :duration 2.5)))
                            :repeat (:count 2 :yoyop t :delay 0.5)))
        :for time := 0.0 :then (+ time delta)
        :for expected :in (read-from-file (system-absolute-pathname "data/timeline.lisp"))
        :do (ute:update delta) (is float-list-equal expected (list time f1 f2))
        :until (ute:finishedp timeline)
        :finally (ute:update delta)))
