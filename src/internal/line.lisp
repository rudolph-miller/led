(in-package :cl-user)
(defpackage led.internal.line
  (:use :cl
        :led.internal.character)
  (:export :make-line
           :line-ichars
           :line-eol-p
           :line-length
           :string-to-line
           :line-ichars-with-padding))
(in-package :led.internal.line)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; line
(defstruct line
  (ichars #() :type array)
  (eol-p nil :type boolean))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; line-length

(defun line-length (line)
  (length (line-ichars line)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; string-to-line

(defun string-to-line (string &optional (eol-p nil))
  (let ((ichars (loop for character across string
                     collecting (character-to-ichar character) into result
                     finally (return (apply #'vector result)))))
    (make-line :ichars ichars :eol-p eol-p)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; line-ichars-with-padding

(defun line-ichars-with-padding (line length)
  (assert (<= (line-length line) length))
  (loop with result = (make-array length :initial-element nil)
        with ichars = (line-ichars line)
        for ichar across ichars
        for i from 0
        do (setf (aref result i) ichar)
           finally (return result)))
