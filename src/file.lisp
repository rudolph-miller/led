(in-package :cl-user)
(defpackage led.file
  (:use :cl)
  (:import-from :led.string
                :string-to-lines
                :lines-to-string)
  (:import-from :led.buffer
                :buffer
                :buffer-name
                :buffer-lines))
(in-package :led.file)

(defun read-file-to-lines (path)
  (if (uiop:file-exists-p path)
      (string-to-lines (uiop:read-file-string path))
      #()))

(defun write-lines-to-file (buffer)
  (check-type buffer file-buffer)
  (with-open-file (stream (file-buffer-path buffer)
                          :direction :output
                          :if-exists :supersede
                          :if-does-not-exist :create)
    (write-string (lines-to-string (buffer-lines buffer)) stream)))

(defclass file-buffer (buffer)
  ((path :accessor file-buffer-path
         :initarg :path)))

(defmethod initialize-instance :after ((buffer file-buffer) &rest initargs)
  (declare (ignore initargs))
  (setf (buffer-name buffer) (format nil "FILE: ~a" (file-buffer-path buffer)))
  (setf (buffer-lines buffer) (read-file-to-lines (file-buffer-path buffer))))
