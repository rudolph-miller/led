(in-package :cl-user)
(defpackage led.internal.mode
  (:use :cl)
  (:export :*modes*
           :*current-mode*))
(in-package :led.internal.mode)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; globals

(defvar *modes* (list :normal :insert :command-line))

(defvar *current-mode* :normal)
