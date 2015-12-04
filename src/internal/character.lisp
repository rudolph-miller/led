(in-package :cl-user)
(defpackage led.internal.character
  (:use :cl)
  (:export :ichar
           :make-ichar
           :ichar-char
           :ichar-attr
           :character-to-ichar
           :ichar-width
           :ichars-width))
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
;; ichar-width

(defun ichar-width (ichar)
  (let ((char (ichar-char ichar)))
    (if (char<= char #\U+007F) 1 2)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ichars-width

(defun ichars-width (ichars)
  (loop with result = 0
        for ichar across ichars
        do (incf result (ichar-width ichar))
        finally (return result)))
