(in-package :cl-user)
(defpackage led.internal.string
  (:use :cl
        :led.internal.character
        :led.internal.bidirectional-link
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
          for line = (insert-next-line got top-line)
            then (insert-next-line got line))
    top-line))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; lines-to-string

(defun lines-chars-length (lines)
  (let ((result 0))
    (iterate-lines lines (lambda (line) (incf result (1+ (line-ichars-length line)))))
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
