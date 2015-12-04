(in-package :cl-user)
(defpackage led.command.window
  (:use :cl
        :led.window)
  (:export :quit
           :q))
(in-package :led.command.window)

(defun quit ()
  (stop-input-loop)
  (close-window))

(defun q ()
  (quit))
