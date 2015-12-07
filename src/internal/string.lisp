(in-package :cl-user)
(defpackage led.internal.string
  (:use :cl
        :led.internal.character
        :led.internal.line)
  (:export :string-to-lines
           :lines-to-string))
(in-package :led.internal.string)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; string-to-lines

(defun string-to-lines (string)
  (loop with stream = (make-string-input-stream string)
        for string = (read-line stream nil nil)
        while string
        for line = (make-line string) then (insert-next-line string line)
        finally (return (next-line line))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; lines-to-string

(defun line-ichars-length (line)
  (1+ (line-length line)))

(defun lines-chars-length (lines)
  (let ((result 0))
    (iterate-lines lines (lambda (line) (incf result (line-ichars-length line))))
    result))

(defun lines-to-string (lines)
  (let ((result (make-string (lines-chars-length lines)))
        (pos 0))
    (iterate-lines lines
                   (lambda (line)
                     (loop for ichar across (line-ichars line)
                           do (setf (elt result pos) (ichar-char ichar))
                              (incf pos)
                           finally (setf (elt result pos) #\NewLine)
                                   (incf pos))))
    result))
