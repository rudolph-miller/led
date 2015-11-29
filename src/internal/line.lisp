(in-package :cl-user)
(defpackage led.internal.line
  (:use :cl)
  (:import-from :led.internal.character
                :character-to-ichar)
  (:export :make-line
           :line-chars
           :line-eol-p
           :line-length
           :string-to-line
           :line-chars-with-padding))
(in-package :led.internal.line)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; line
(defstruct line
  (chars #() :type array)
  (eol-p nil :type boolean))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; line-length

(defun line-length (line)
  (length (line-chars line)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; string-to-line

(defun string-to-line (string &optional (eol-p nil))
  (let ((chars (loop for character across string
                     collecting (character-to-ichar character) into result
                     finally (return (apply #'vector result)))))
    (make-line :chars chars :eol-p eol-p)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; line-chars-with-padding

(defun line-chars-with-padding (line length)
  (assert (<= (line-length line) length))
  (loop with result = (make-array length :initial-element nil)
        with chars = (line-chars line)
        for ichar across chars
        for i from 0
        do (setf (aref result i) ichar)
           finally (return result)))
