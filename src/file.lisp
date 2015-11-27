(in-package :cl-user)
(defpackage led.file
  (:use :cl)
  (:import-from :led.buffer
                :buffer
                :buffer-name
                :buffer-lines
                :set-buffer-content
                :get-buffer-content))
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
    (write-string (get-buffer-content buffer) stream)))

(defclass file-buffer (buffer)
  ((path :accessor file-buffer-path
         :initarg :path)))

(defmethod initialize-instance :after ((buffer file-buffer) &rest initargs)
  (declare (ignore initargs))
  (let ((path (file-buffer-path buffer)))
    (set-buffer-content buffer (if (uiop:file-exists-p path)
                                   (uiop:read-file-string path)
                                   #()))
    (setf (buffer-name buffer)
          (format nil "FILE: ~a" path))))
