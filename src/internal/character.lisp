(in-package :cl-user)
(defpackage led.internal.character
  (:use :cl)
  (:export :ichar
           :make-ichar
           :ichar-char
           :ichar-attr
           :character-to-ichar
           :ichar-length))
(in-package :led.internal.character)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ichar

(defstruct ichar
  char
  attr)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; character-to-ichar

(defun character-to-ichar (character)
  (make-ichar :val character))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ichar-length

(defun ichar-length (ichar)
  (let ((char (ichar-char ichar)))
    (if (char<= char #\U+007F) 1 2)))
