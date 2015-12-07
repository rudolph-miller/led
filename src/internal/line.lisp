(in-package :cl-user)
(defpackage led.internal.line
  (:use :cl
        :led.internal.character
        :led.internal.bidirectional-link)
  (:export :make-line
           :line-index
           :line-ichars
           :line-string
           :ichar-position
           :make-top-line
           :line-length
           :line-ichars-length
           :insert-prev-line
           :insert-next-line
           :delete-line
           :delete-ichar-of-line
           :replace-ichar-of-line
           :insert-ichar-to-line
           :iterate-lines))
(in-package :led.internal.line)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; line

(defstruct (line (:include bd)
                 (:constructor %make-line)))

(defun line-ichars (line)
  (bd-value line))

(defun line-string (line)
  (with-output-to-string (stream)
    (loop for ichar across (line-ichars line)
          do (write-char (ichar-char ichar) stream))))

(defmethod print-object ((object line) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream
            ":INDEX ~a :CONTENTS ~a"
            (bd-index object)
            (line-string object))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ichar-position

(defvar *ichar-line-table* (make-hash-table :test #'eq))

(defun ichar-position (ichar)
  (let* ((line (gethash ichar *ichar-line-table*))
         (x (loop for i across (line-ichars line)
                  for index from 0
                  when (eq i ichar) do (return index))))
    (cons x line)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; make-top-line

(defun make-top-line ()
  (make-top-bd))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; make-line

(defun make-line (ichars)
  (let* ((ichars (etypecase ichars
                   (string (loop for character across ichars
                                 collecting (character-to-ichar character)))
                   ((array ichar) ichars)
                   (cons ichars)
                   (null nil)))
         (adjustable (make-array (length ichars)
                                 :initial-contents ichars
                                 :adjustable t))
         (line (%make-line :value adjustable)))
    (loop for ichar across adjustable
          do (setf (gethash ichar *ichar-line-table*) line))
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
  (delete-bd line))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; line-length

(defun line-length (top-line)
  (bd-length top-line))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; line-ichars-length

(defun line-ichars-length (line)
  (length (line-ichars line)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; delete-ichar-of-line

(defun delete-ichar-of-line (x line)
  (let ((ichars (line-ichars line)))
    (setf (subseq ichars x) (subseq ichars (1+ x)))
    (adjust-array ichars (1- (length ichars)))
    line))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; replace-ichar-of-line

(defun replace-ichar-of-line (ichar x line)
  (let ((ichars (line-ichars line)))
    (setf (aref ichars x) ichar)
    (setf (gethash ichar *ichar-line-table*) line)
    line))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; insert-ichar-to-line

(defun insert-ichar-to-line (ichar x line)
  (let ((ichars (line-ichars line)))
    (adjust-array ichars (1+ (length ichars)))
    (setf (subseq ichars (1+ x)) (subseq ichars x))
    (setf (aref ichars x) ichar)
    (setf (gethash ichar *ichar-line-table*) line)
    line))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; iterate-lines

(defun iterate-lines (top-line fn)
  (iterate-to-end top-line fn))
