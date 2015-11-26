(in-package :cl-user)
(defpackage led.line
  (:use :cl)
  (:import-from :led.util
                :make-vector-with)
  (:import-from :led.character
                :character-to-ichar)
  (:export :*max-line-width*
           :make-line
           :string-to-line
           :migrate-line-to-line))
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

(defun migrate-line-to-line (line target start)
  (let* ((line-chars (line-chars line))
         (target-chars (line-chars target))
         (resurt-chars (make-vector-with (max (+ (length line-chars) start)
                                              (length target-chars))
                                         #'(lambda () (character-to-ichar #\Space)))))
    (setq resurt-chars (replace resurt-chars target-chars))
    (setq resurt-chars (replace resurt-chars line-chars :start1 start))
    (make-line resurt-chars)))
