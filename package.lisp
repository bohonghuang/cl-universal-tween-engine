(defpackage universal-tween-engine
  (:use #:cl #:alexandria)
  (:nicknames #:ute)
  (:export
   #:start
   #:pause-all
   #:resume-all
   #:kill-all
   #:update
   #:tween
   #:timeline
   #:pause
   #:kill
   #:resume
   #:back-in
   #:back-out
   #:back-inout
   #:bounce-out
   #:bounce-in
   #:bounce-inout
   #:circ-in
   #:circ-out
   #:circ-inout
   #:cubic-in
   #:cubic-out
   #:cubic-inout
   #:elastic-in
   #:elastic-out
   #:elastic-inout
   #:expo-in
   #:expo-out
   #:expo-inout
   #:linear-inout
   #:quad-in
   #:quad-out
   #:quad-inout
   #:quart-in
   #:quart-out
   #:quart-inout
   #:quint-in
   #:quint-out
   #:quint-inout
   #:sine-in
   #:sine-out
   #:catmull-rom
   #:linear
   #:killedp
   #:pausedp
   #:startedp
   #:finishedp
   #:duration
   #:full-duration
   #:+easings+
   #:+paths+
   #:*tween-manager*
   #:tween-manager
   #:make-tween-manager
   #:callback
   #:callback-type
   #:+callback-begin+
   #:+callback-start+
   #:+callback-end+
   #:+callback-complete+
   #:+callback-back-begin+
   #:+callback-back-start+
   #:+callback-back-end+
   #:+callback-back-complete+
   #:+callback-any-forward+
   #:+callback-any-backward+
   #:+callback-any+
   #:+callback-none+
   #:add-child
   #:add-waypoint
   #:waypoints-limit
   #:combined-attributes-limit))

(in-package #:universal-tween-engine)

(deftype u8 () `(unsigned-byte 8))

(deftype i32 () `(signed-byte 32))

(deftype f32 () `single-float)

(deftype i32->f32 ()
  `(single-float #.(coerce (- (truncate (expt 2 32) 2)) 'single-float)
                 #.(coerce (1- (truncate (expt 2 32) 2)) 'single-float)))
