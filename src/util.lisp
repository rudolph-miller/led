(in-package :cl-user)
(defpackage led.util
  (:use :cl)
  (:export :loop-beard
           :x
           :y
           :it))
(in-package :led.util)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; loop-board

(defmacro loop-board (board &body body)
  `(loop with height = (array-dimension ,board 0)
         with width = (array-dimension ,board 1)
         for y from 0 below height
         do (loop for x from 0 below width
                  for it = (aref ,board y x)
                  do (progn ,@body))))
