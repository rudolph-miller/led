(in-package :cl-user)
(defpackage led.internal.bidirectional-link
  (:use :cl)
  (:export :bd
           :make-bd
           :bd-length
           :bd-index+
           :bd-index-
           :bd-index=
           :bd-index<
           :bd-index<=
           :bd-index>
           :bd-index>=
           :prev
           :next
           :bd-value
           :bd-index
           :iterate-to-end
           :get-by-index
           :insert-prev
           :insert-next
           :delete-bd))
(in-package :led.internal.bidirectional-link)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; globals

(defvar +top-index+ +top-index+)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; bd

(defstruct bd
  prev
  next
  value
  index)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; make-new-bd

(defun make-new-bd ()
  (let ((bd (make-bd :index +top-index+)))
    (setf (bd-prev bd) bd)
    (setf (bd-next bd) bd)
    bd))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; top-bd-p

(defun top-bd-p (bd)
  (eql (bd-index bd) +top-index+))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; bd-legth

(defun bd-length (bd)
  (assert (top-bd-p bd))
  (let ((prev (bd-prev bd)))
    (if (top-bd-p prev)
        0
        (1+ (bd-index prev)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; print-object

(defmethod print-object ((object bd) stream)
  (if (top-bd-p object)
      (print-unreadable-object (object stream :identity t)
        (format stream "TOP-BD :LENGTH ~a" (bd-length object)))
      (print-unreadable-object (object stream :type t :identity t)
        (format stream ":VALUE ~a :INDEX ~a" (bd-value object) (bd-index object)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; bd-index culculation

(defmacro def-bd-index-calc (symbol)
  `(defun ,(intern (format nil "BD-INDEX~a" symbol)) (&rest bds)
     (apply ',symbol (mapcar #'bd-index bds))))

(def-bd-index-calc +)
(def-bd-index-calc -)
(def-bd-index-calc =)
(def-bd-index-calc <)
(def-bd-index-calc <=)
(def-bd-index-calc >)
(def-bd-index-calc >=)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; prev and next

(defun prev (bd)
  (let ((prev (bd-prev bd)))
    (unless (top-bd-p prev)
      prev)))

(defun next (bd)
  (let ((next (bd-next bd)))
    (unless (top-bd-p next)
      next)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; iterate

(defun iterate-to-end (bd fn)
  (loop for item = (if (top-bd-p bd) (next bd) bd)
          then (next item)
        while item
        do (funcall fn item)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; get-by-index

(defun get-by-index (index bd)
  (assert (> index (bd-index bd)))
  (iterate-to-end
   bd
   (lambda (item)
     (when (= (bd-index item) index)
       (return-from get-by-index item)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; insert-prev

(defun insert-prev (new-bd bd)
  (assert (not (top-bd-p bd)))
  (let ((prev (bd-prev bd)))
    (setf (bd-index new-bd) (bd-index bd))
    (setf (bd-prev new-bd) (bd-prev prev))
    (setf (bd-next new-bd) bd)
    (setf (bd-prev bd) new-bd)
    (setf (bd-next prev) new-bd)
    (iterate-to-end bd #'(lambda (bd) (incf (bd-index bd))))
    new-bd))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; insert-next

(defun insert-next (new-bd bd)
  (let ((next (bd-next bd)))
    (setf (bd-index new-bd) (if (top-bd-p bd)
                                0
                                (1+ (bd-index bd))))
    (setf (bd-prev new-bd) bd)
    (setf (bd-next new-bd) next)
    (unless (top-bd-p next)
      (iterate-to-end next #'(lambda (bd) (incf (bd-index bd)))))
    (setf (bd-next bd) new-bd)
    (setf (bd-prev next) new-bd)
    new-bd))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; delete-bd

(defun delete-bd (bd)
  (let ((prev (bd-prev bd))
        (next (bd-next bd)))
    (unless (zerop (bd-index next))
      (iterate-to-end next #'(lambda (bd) (decf (bd-index bd)))))
    (setf (bd-next prev) next)
    (setf (bd-prev next) prev)
    t))
