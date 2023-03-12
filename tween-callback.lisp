(in-package #:universal-tween-engine)

(defconstant +tween-callback-none+ #x00)
(defconstant +tween-callback-begin+ #x01)
(defconstant +tween-callback-start+ #x02)
(defconstant +tween-callback-end+ #x04)
(defconstant +tween-callback-complete+ #x08)
(defconstant +tween-callback-back-begin+ #x10)
(defconstant +tween-callback-back-start+ #x20)
(defconstant +tween-callback-back-end+ #x40)
(defconstant +tween-callback-back-complete+ #x80)
(defconstant +tween-callback-any-forward+ #x0F)
(defconstant +tween-callback-any-backward+ #xF0)
(defconstant +tween-callback-any+ #xFF)

(declaim (type u8 *tween-callback-type*))
(defvar *tween-callback-type* #x00)

(deftype tween-callback () `(function ()))
