(in-package :cl-user)
(defpackage led.internal.line
  (:use :cl
        :led.internal.bidirectional-link
        :led.internal.character
        :led.internal.bdl-ichar)
  (:export :make-line
           :line-index
           :line-bdl-ichars
           :line-string
           :bdl-ichar-line
           :make-top-line
           :line-length
           :line-bdl-ichars-length
           :insert-prev-line
           :insert-next-line
           :delete-line
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
    (when string
      (loop for char across string
            for got = (insert-next-bdl-ichar char bdl-ichar)
              then (insert-next-bdl-ichar char got)
            do (setf (gethash got *bdl-ichar-line-table*) line)))
    line))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; insert-prev-line

(defun insert-prev-line (object line)
  (let ((new-line (if (typep object 'line)
                      object
                      (make-line object))))
    (insert-prev new-line line)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; insert-next-line

(defun insert-next-line (object line)
  (let ((new-line (if (typep object 'line)
                      object
                      (make-line object))))
    (insert-next new-line line)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; delete-line

(defun delete-line (line)
  (delete-bdl line))


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
