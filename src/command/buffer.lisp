(in-package :cl-user)
(defpackage led.command.buffer
  (:use :led.buffer)
  (:import-from :cl
                :defun
                :make-instance)
  (:export :write
           :w
           :edit
           :e))
(in-package :led.command.buffer)

(defun write ()
  (write-buffer-to-file))

(defun w ()
  (write))

(defun edit (path)
  (make-instance 'file-buffer :path path))

(defun e (path)
  (edit path))
