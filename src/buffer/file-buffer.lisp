(in-package :cl-user)
(defpackage led.buffer.file-buffer
  (:use :cl
        :led.internal
        :led.buffer.buffer
        :led.buffer.command-line-buffer)
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
;; buffer-status

(defmethod buffer-status ((buffer file-buffer))
  (let* ((path (file-buffer-path buffer))
         (bdl-ichar (buffer-cursor buffer))
         (current-line (bdl-ichar-line bdl-ichar))
         (x (bdl-ichar-index bdl-ichar))
         (y (line-index current-line)))
    (format nil "FILE: ~a (~a,~a)" path x y)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; write-buffer-to-file

(defun write-buffer-to-file (&optional (buffer *current-buffer*))
  (check-type buffer file-buffer)
  (let ((path (file-buffer-path buffer))
        (content (buffer-content buffer)))
    (with-open-file (stream path
                            :direction :output
                            :if-exists :supersede
                            :if-does-not-exist :create)
      (write-string content stream)
      (on-command-line (format nil
                               "\"~a\" ~aL, ~aC written"
                               (file-buffer-path buffer)
                               (length (buffer-lines buffer))
                               (length content))))))
