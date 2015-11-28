(in-package :cl-user)
(defpackage led.file
  (:use :cl)
  (:import-from :led.string
                :string-to-lines)
  (:import-from :led.buffer
                :*current-buffer*
                :buffer
                :buffer-name
                :buffer-lines
                :set-buffer-content
                :buffer-content))
(in-package :led.file)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; file-buffer

(defclass file-buffer (buffer)
  ((path :accessor file-buffer-path
         :initarg :path)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; initialize

(defun read-file-to-lines (path)
  (if (uiop:file-exists-p path)
      (string-to-lines (uiop:read-file-string path))
      #()))

(defmethod initialize-instance :after ((buffer file-buffer) &rest initargs)
  (declare (ignore initargs))
  (let ((path (file-buffer-path buffer)))
    (set-buffer-content (if (uiop:file-exists-p path)
                                   (uiop:read-file-string path)
                                   #())
                        buffer)
    (setf (buffer-name buffer)
          (format nil "FILE: ~a" path))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; write-buffer-to-file

(defun write-buffer-to-file (&optional (buffer *current-buffer*))
  (check-type buffer file-buffer)
  (with-open-file (stream (file-buffer-path buffer)
                          :direction :output
                          :if-exists :supersede
                          :if-does-not-exist :create)
    (write-string (buffer-content buffer) stream)))
