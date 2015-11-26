(in-package :cl-user)
(defpackage led.character
  (:use :cl)
  (:export :ichar
           :make-ichar
           :ichar-val
           :ichar-attr
           :character-to-ichar))
(in-package :led.character)

(defstruct ichar
  val
  attr)

(defun character-to-ichar (character)
  (make-ichar :val character))
