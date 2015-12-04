(in-package :cl-user)
(defpackage led
  (:use :cl
        :led.window
        :led.buffer))
(in-package :led)

(defun init (&optional path)
  (make-window)
  (make-instance 'command-line-buffer)
  (when path
    (make-instance 'file-buffer :path path))
  (start-input-loop))
