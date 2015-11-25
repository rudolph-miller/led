(in-package :cl-user)
(defpackage led.character
  (:use :cl))
(in-package :led.character)

(defstruct ichar
  val
  attr)

(defun character-to-ichar (character)
  (make-ichar :val character))
