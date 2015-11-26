(in-package :cl-user)
(defpackage led.character
  (:use :cl)
  (:export :ichar
           :ichar-val
           :ichar-attr))
(in-package :led.character)

(defstruct ichar
  val
  attr)
