(in-package :cl-user)
(defpackage led.internal.bdl-ichar
  (:use :cl
        :led.internal.bidirectional-link
        :led.internal.ichar)
  (:export :bdl-ichar
           :bdl-ichar-index
           :bdl-ichar-ichar
           :make-top-bdl-ichar
           :make-bdl-ichar
           :bdl-ichar-length
           :iterate-bdl-ichars
           :bdl-ichars-to-ichars))
(in-package :led.internal.bdl-ichar)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; bdl-ichar

(defstruct (bdl-ichar (:include bdl)
                      (:constructor %make-bdl-ichar)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; bdl-ichar-ichar

(defun bdl-ichar-ichar (bdl-ichar)
  (bdl-value bdl-ichar))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; make-top-bdl-ichar

(defun make-top-bdl-ichar ()
  (make-top-bdl))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; make-bdl-ichar

(defun make-bdl-ichar (object)
  (let ((ichar (etypecase object
                 (character (character-to-ichar object))
                 (ichar object))))
    (%make-bdl-ichar :value ichar)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; bdl-ichar-length

(defun bdl-ichar-length (top-bdl-ichar)
  (bdl-length top-bdl-ichar))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; iterate-bdl-ichars

(defun iterate-bdl-ichars (top-bdl-ichar fn)
  (iterate-bdl top-bdl-ichar fn))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; bdl-ichars-to-ichars

(defun bdl-ichars-to-ichars (top-bdl-ichar)
  (let ((result (make-array (bdl-ichar-length top-bdl-ichar)))
        (position 0))
    (iterate-bdl-ichars
     top-bdl-ichar
     (lambda (bdl-ichar)
       (setf (aref result position) (bdl-ichar-ichar bdl-ichar))
       (incf position)))
    result))
