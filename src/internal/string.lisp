(in-package :cl-user)
(defpackage led.internal.string
  (:use :cl
        :led.internal.ichar
        :led.internal.bidirectional-link
        :led.internal.bdl-ichar
        :led.internal.line)
  (:export :string-to-lines
           :lines-to-string))
(in-package :led.internal.string)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; string-to-lines

(defun string-to-lines (string)
  (let ((top-line (make-top-line)))
    (loop with stream = (make-string-input-stream string)
          for got = (read-line stream nil nil)
          while got
          for line = (insert-next (make-line got) top-line)
            then (insert-next (make-line got) line))
    top-line))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; lines-to-string

(defun lines-chars-length (lines)
  (let ((result 0))
    (iterate-lines
     lines
     (lambda (line)
       (incf result (1+ (line-bdl-ichars-length line)))))
    result))

(defun lines-to-string (lines)
  (let ((result (make-string (lines-chars-length lines)))
        (pos 0))
    (iterate-lines
     lines
     (lambda (line)
       (iterate-bdl-ichars
        (line-top-bdl-ichar line)
        (lambda (bdl-ichar)
          (setf (elt result pos) (ichar-char (bdl-ichar-ichar bdl-ichar)))
          (incf pos)))
        (setf (elt result pos) #\NewLine)
        (incf pos)))
    result))
