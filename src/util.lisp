(in-package :cl-user)
(defpackage led.util
  (:use :cl)
  (:export :make-vector-with))
(in-package :led.util)

(defun make-vector-with (n fn)
  (loop repeat n
        collecting (funcall fn) into result
        finally (return (apply #'vector result))))
