(in-package :cl-user)
(defpackage led.line
  (:use :cl)
  (:export :*max-line-width*
           :make-line))
(in-package :led.line)

(defparameter *max-line-width* nil)

(defstruct (line (:constructor %make-line))
  chars)

(defun make-line (&optional (chars (make-array 0)))
  (when *max-line-width* (assert (<= (length chars) *max-line-width*)))
  (%make-line :chars chars))
