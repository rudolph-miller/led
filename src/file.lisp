(in-package :cl-user)
(defpackage led.file
  (:use :cl)
  (:import-from :led.string
                :string-to-lines)
  (:import-from :led.buffer
                :buffer
                :buffer-name
                :buffer-lines))
(in-package :led.file)

(defun read-file-to-lines (path)
  (apply #'vector (string-to-lines (uiop:read-file-string path))))

(defclass file-buffer (buffer)
  ((path :accessor file-buffer-path
         :initarg :path)))

(defmethod initialize-instance :after ((buffer file-buffer) &rest initargs)
  (declare (ignore initargs))
  (setf (buffer-name buffer) (format nil "FILE: ~a" (file-buffer-path buffer)))
  (setf (buffer-lines buffer) (read-file-to-lines (file-buffer-path buffer))))
