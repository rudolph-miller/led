(in-package :cl-user)
(defpackage led.file
  (:use :cl)
  (:import-from :led.string
                :string-to-lines))
(in-package :led.file)

(defstruct (file (:constructor %make-file))
  lines
  path)

(defun read-file-lines (file)
  (string-to-lines (uiop:read-file-string (file-path file))))

(defun make-file (path)
  (let ((file (%make-file :path path)))
    (setf (file-lines file) (read-file-lines file))
    file))
