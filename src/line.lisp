(in-package :cl-user)
(defpackage led.line
  (:use :cl)
  (:export :make-line))
(in-package :led.line)

(defparameter *max-line-width* nil)

(defstruct (line (:constructor %make-line))
  chars)

(defun make-line (&optional chars)
  (assert (<= (length chars) *max-line-width*))
  (%make-line :chars chars))
