(in-package :cl-user)
(defpackage led.line
  (:use :cl)
  (:import-from :led.character
                :character-to-ichar)
  (:export :*max-line-width*
           :make-line))
(in-package :led.line)

(defparameter *max-line-width* nil)

(defstruct (line (:constructor %make-line))
  (chars #() :type array))

(defun make-line (&optional (chars #()))
  (when *max-line-width* (assert (<= (length chars) *max-line-width*)))
  (%make-line :chars chars))

(defun string-to-line (string)
  (let ((chars (loop for character across string
                     collecting (character-to-ichar character) into result
                     finally (return (apply #'vector result)))))
    (make-line chars)))
