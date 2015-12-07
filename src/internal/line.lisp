(in-package :cl-user)
(defpackage led.internal.line
  (:use :cl
        :led.internal.character
        :led.internal.bidirectional-link)
  (:export :make-line
           :line-ichars
           :prev-line
           :next-line
           :line-string
           :line-length
           :insert-prev-line
           :insert-next-line
           :delete-line
           :delete-ichar-of-line
           :replace-ichar-of-line
           :insert-ichar-to-line))
(in-package :led.internal.line)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; line

(defstruct (line (:include bd)
                 (:constructor %make-line)))

(defun line-ichars (line)
  (bd-value line))

(defun prev-line (line)
  (bd-prev line))

(defun next-line (line)
  (bd-next line))

(defun line-string (line)
  (with-output-to-string (stream)
    (loop for ichar across (line-ichars line)
          do (write-char (ichar-char ichar) stream))))

(defmethod print-object ((object line) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream ":INDEX ~a :CONTENTS ~a" (bd-index object) (line-string object))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; make-line

(defun make-line (ichars)
  (let* ((ichars (etypecase ichars
                   (string (loop for character across ichars
                                 collecting (character-to-ichar character)))
                   ((array ichar) ichars)
                   (cons ichars)))
         (adjustable (make-array (length ichars)
                                 :initial-contents ichars
                                 :adjustable t))
         (line (%make-line :value adjustable)))
    (setf (bd-index line) 0)
    (setf (bd-next line) line)
    (setf (bd-prev line) line)
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

(defun line-length (line)
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
    line))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; insert-ichar-to-line

(defun insert-ichar-to-line (ichar x line)
  (let ((ichars (line-ichars line)))
    (adjust-array ichars (1+ (length ichars)))
    (setf (subseq ichars (1+ x)) (subseq ichars x))
    (setf (aref ichars x) ichar)
    line))
