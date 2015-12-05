(in-package :cl-user)
(defpackage led.internal.bidirectional-list
  (:use :cl)
  (:export :bd
           :make-new-bd
           :iterate-to-end
           :iterate-n-times
           :insert-prev
           :insert-next))
(in-package :led.internal.bidirectional-list)

(defstruct bd
  prev
  next
  value
  index)

(defmethod print-object ((object bd) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream ":VALUE ~a :INDEX ~a" (bd-value bd) (bd-index object))))

(defun make-new-bd (item)
  (let ((bd (make-bd :value item
                     :index 0)))
    (setf (bd-prev bd) bd)
    (setf (bd-next bd) bd)
    bd))

(defun iterate-to-end (bd fn)
  (loop for item = bd then (bd-next item)
        for next = (bd-next item)
        do (funcall fn item)
        until (zerop (bd-index next))))

(defun iterate-n-times (bd n fn)
  (loop repeat n
        for item = bd then (bd-next item)
        for next = (bd-next item)
        do (funcall fn item)))

(defun insert-prev (item bd)
  (let* ((prev (bd-prev bd))
         (new-bd (make-bd :prev (bd-prev prev)
                          :next bd
                          :value item
                          :index (bd-index bd))))
    (setf (bd-next prev) new-bd)
    (setf (bd-prev bd) new-bd)
    (iterate-to-end bd #'(lambda (bd) (incf (bd-index bd))))
    new-bd))

(defun insert-next (item bd)
  (let* ((next (bd-next bd))
         (new-bd (make-bd :prev bd
                          :next next
                          :value item
                          :index (1+ (bd-index bd)))))
    (unless (eq next (bd-prev bd))
      (iterate-to-end next #'(lambda (bd) (print bd) (incf (bd-index bd)))))
    (setf (bd-next bd) new-bd)
    (setf (bd-prev next) new-bd)
    new-bd))
