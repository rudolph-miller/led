(in-package :cl-user)
(defpackage led.internal.line
  (:use :cl
        :led.internal.bidirectional-link
        :led.internal.ichar
        :led.internal.bdl-ichar)
  (:export :line-index
           :line-top-bdl-ichar
           :line-string
           :bdl-ichar-line
           :make-top-line
           :make-line
           :line-length
           :line-bdl-ichars-length
           :iterate-lines))
(in-package :led.internal.line)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; line

(defstruct (line (:include bdl)
                 (:constructor %make-line)))

(defun line-top-bdl-ichar (line)
  (bdl-value line))

(defun line-string (line)
  (with-output-to-string (stream)
    (iterate-bdl-ichars
     (line-top-bdl-ichar line)
     (lambda (bdl-ichar)
       (write-char (ichar-char (bdl-ichar-ichar bdl-ichar)) stream)))))

(defmethod print-object ((object line) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream
            ":INDEX ~a :CONTENTS ~a"
            (bdl-index object)
            (line-string object))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; bdl-ichar-line

(defvar *bdl-ichar-line-table* (make-hash-table :test #'eq))

(defun bdl-ichar-line (bdl-ichar)
  (gethash bdl-ichar *bdl-ichar-line-table*))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; make-top-line

(defun make-top-line ()
  (make-top-bdl))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; make-line

(defun make-line (string)
  (let* ((bdl-ichar (make-top-bdl-ichar))
         (line (%make-line :value bdl-ichar)))
    (when (and string (> (length string) 0))
      (loop for char across string
            for got = (insert-next (make-bdl-ichar char) bdl-ichar)
              then (insert-next (make-bdl-ichar char) got)
            do (setf (gethash got *bdl-ichar-line-table*) line)))
    line))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; line-length

(defun line-length (top-line)
  (bdl-length top-line))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; line-bdl-ichars-length

(defun line-bdl-ichars-length (line)
  (bdl-ichar-length (line-top-bdl-ichar line)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; iterate-lines

(defun iterate-lines (top-line fn)
  (iterate-bdl top-line fn))
