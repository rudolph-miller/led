(in-package :cl-user)
(defpackage led.internal.string
  (:use :cl)
  (:import-from :led.internal.character
                :ichar-val)
  (:import-from :led.internal.line
                :line-chars
                :line-eol-p
                :line-length
                :string-to-line)
  (:export :string-to-lines
           :lines-to-string))
(in-package :led.internal.string)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; string-to-lines

(defun string-to-lines (string)
  (loop with stream = (make-string-input-stream string)
        for line = (read-line stream nil nil)
        while line
        collecting (string-to-line line t) into result
        finally (return (apply #'vector result))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; lines-to-string

(defun line-chars-length (line)
  (let ((chars-len (length (line-chars line))))
    (if (line-eol-p line)
        (1+ chars-len)
        chars-len)))

(defun lines-chars-length (lines)
  (reduce #'+ (loop for line across lines collecting (line-chars-length line))))

(defun lines-to-string (lines)
  (loop with result = (make-string (lines-chars-length lines))
        with pos = 0
        for line across lines
        do (loop for ichar across (line-chars line)
                 do (setf (elt result pos) (ichar-val ichar))
                    (incf pos)
                 finally (when (line-eol-p line)
                           (setf (elt result pos) #\NewLine)
                           (incf pos)))
        finally (return result)))
