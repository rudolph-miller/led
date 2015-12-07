(in-package :cl-user)
(defpackage led.internal.line
  (:use :cl
        :led.internal.character)
  (:export :make-line
           :line-ichars
           :line-eol-p
           :line-length
           :delete-ichar-of-line
           :replace-ichar-of-line
           :insert-ichar-to-line
           :string-to-line))
(in-package :led.internal.line)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; line
(defstruct (line (:constructor %make-line))
  (ichars #() :type array)
  (eol-p nil :type boolean))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; make-line

(defun make-line (ichars &optional eol-p)
  (let ((adjustable (make-array (length ichars)
                                :initial-contents ichars
                                :adjustable t)))
    (%make-line :ichars adjustable :eol-p eol-p)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; line-length

(defun line-length (line)
  (length (line-ichars line)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; delete-ichar-of-line

(defun delete-ichar-of-line (x line)
  (let ((ichars (line-ichars line)))
    (setf (subseq ichars x) (subseq ichars (1+ x)))
    (adjust-array ichars (1- (length ichars)))
    line))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; replace-ichar-of-line

(defun replace-ichar-of-line (ichar x line)
  (let ((ichars (line-ichars line)))
    (setf (aref ichars x) ichar)
    line))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; insert-ichar-to-line

(defun insert-ichar-to-line (ichar x line)
  (let ((ichars (line-ichars line)))
    (adjust-array ichars (1+ (length ichars)))
    (setf (subseq ichars (1+ x)) (subseq ichars x))
    (setf (aref ichars x) ichar)
    line))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; string-to-line

(defun string-to-line (string &optional (eol-p nil))
  (let ((ichars (loop for character across string
                     collecting (character-to-ichar character) into result
                     finally (return (apply #'vector result)))))
    (make-line ichars eol-p)))
