(in-package :cl-user)
(defpackage led.buffer.file-buffer
  (:use :cl)
  (:import-from :led.internal.string
                :string-to-lines)
  (:import-from :led.buffer.buffer
                :*current-buffer*
                :buffer
                :buffer-name
                :buffer-lines
                :set-buffer-content
                :buffer-content)
  (:export :file-buffer
           :write-buffer-to-file))
(in-package :led.buffer.file-buffer)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; file-buffer

(defclass file-buffer (buffer)
  ((path :accessor file-buffer-path
         :initarg :path)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; initialize

(defmethod initialize-instance :after ((buffer file-buffer) &rest initargs)
  (declare (ignore initargs))
  (let ((path (file-buffer-path buffer)))
    (set-buffer-content (and (uiop:file-exists-p path)
                             (uiop:read-file-string path))
                        buffer)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; buffer-name

(defmethod buffer-name ((buffer file-buffer))
  (let ((path (file-buffer-path buffer)))
    (format nil "FILE: ~a" path)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; write-buffer-to-file

(defun write-buffer-to-file (&optional (buffer *current-buffer*))
  (check-type buffer file-buffer)
  (with-open-file (stream (file-buffer-path buffer)
                          :direction :output
                          :if-exists :supersede
                          :if-does-not-exist :create)
    (write-string (buffer-content buffer) stream)))
