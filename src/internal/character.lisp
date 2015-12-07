(in-package :cl-user)
(defpackage led.internal.character
  (:use :cl)
  (:export :ichar
           :make-ichar
           :ichar-char
           :ichar-attr
           :character-to-ichar
           :char-width
           :ichar-width
           :ichars-width
           :ichars-with-padding))
(in-package :led.internal.character)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ichar

(defstruct ichar
  char
  attr)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; character-to-ichar

(defun character-to-ichar (character)
  (make-ichar :char character))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; char-width

(defun char-width (char)
  (if (char<= char #\U+007F) 1 2))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ichar-width

(defun ichar-width (ichar)
  (let ((char (ichar-char ichar)))
    (char-width char)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ichars-width

(defun ichars-width (ichars)
  (loop with result = 0
        for ichar across ichars
        do (incf result (ichar-width ichar))
        finally (return result)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ichars-with-padding

(defun ichars-with-padding (ichars length)
  (assert (<= (length ichars) length))
  (loop with result = (make-array length :initial-element nil)
        for ichar across ichars
        for i from 0
        do (setf (aref result i) ichar)
           finally (return result)))
