(in-package :cl-user)
(defpackage led.internal.bidirectional-link
  (:use :cl)
  (:export :bdl
           :make-bdl
           :make-top-bdl
           :top-bdl-p
           :bdl-length
           :bdl-index+
           :bdl-index-
           :bdl-index=
           :bdl-index<
           :bdl-index<=
           :bdl-index>
           :bdl-index>=
           :prev
           :next
           :bdl-value
           :bdl-index
           :iterate-to-end
           :get-by-index
           :insert-prev
           :insert-next
           :delete-bdl))
(in-package :led.internal.bidirectional-link)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; globals

(defvar +top-index+ :dummy)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; bdl

(defstruct bdl
  prev
  next
  value
  index)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; make-new-bdl

(defun make-top-bdl ()
  (let ((bdl (make-bdl :index +top-index+)))
    (setf (bdl-prev bdl) bdl)
    (setf (bdl-next bdl) bdl)
    bdl))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; top-bdl-p

(defun top-bdl-p (bdl)
  (eql (bdl-index bdl) +top-index+))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; bdl-legth

(defun bdl-length (bdl)
  (assert (top-bdl-p bdl))
  (let ((prev (bdl-prev bdl)))
    (if (top-bdl-p prev)
        0
        (1+ (bdl-index prev)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; print-object

(defmethod print-object ((object bdl) stream)
  (if (top-bdl-p object)
      (print-unreadable-object (object stream :identity t)
        (format stream "TOP-BDL :LENGTH ~a" (bdl-length object)))
      (print-unreadable-object (object stream :type t :identity t)
        (format stream ":VALUE ~a :INDEX ~a" (bdl-value object) (bdl-index object)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; bdl-index culculation

(defmacro def-bdl-index-calc (symbol)
  `(defun ,(intern (format nil "BDL-INDEX~a" symbol)) (&rest bdls)
     (apply ',symbol (mapcar #'bdl-index bdls))))

(def-bdl-index-calc +)
(def-bdl-index-calc -)
(def-bdl-index-calc =)
(def-bdl-index-calc <)
(def-bdl-index-calc <=)
(def-bdl-index-calc >)
(def-bdl-index-calc >=)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; prev and next

(defun prev (bdl)
  (let ((prev (bdl-prev bdl)))
    (unless (top-bdl-p prev)
      prev)))

(defun next (bdl)
  (let ((next (bdl-next bdl)))
    (unless (top-bdl-p next)
      next)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; iterate

(defun iterate-to-end (bdl fn)
  (loop for item = (if (top-bdl-p bdl) (next bdl) bdl)
          then (next item)
        while item
        do (funcall fn item)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; get-by-index

(defun get-by-index (index bdl)
  (assert (> index (bdl-index bdl)))
  (iterate-to-end
   bdl
   (lambda (item)
     (when (= (bdl-index item) index)
       (return-from get-by-index item)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; insert-prev

(defun insert-prev (new-bdl bdl)
  (assert (not (top-bdl-p bdl)))
  (let ((prev (bdl-prev bdl)))
    (setf (bdl-index new-bdl) (bdl-index bdl))
    (setf (bdl-prev new-bdl) (bdl-prev prev))
    (setf (bdl-next new-bdl) bdl)
    (setf (bdl-prev bdl) new-bdl)
    (setf (bdl-next prev) new-bdl)
    (iterate-to-end bdl #'(lambda (bdl) (incf (bdl-index bdl))))
    new-bdl))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; insert-next

(defun insert-next (new-bdl bdl)
  (let ((next (bdl-next bdl)))
    (setf (bdl-index new-bdl) (if (top-bdl-p bdl)
                                0
                                (1+ (bdl-index bdl))))
    (setf (bdl-prev new-bdl) bdl)
    (setf (bdl-next new-bdl) next)
    (unless (top-bdl-p next)
      (iterate-to-end next #'(lambda (bdl) (incf (bdl-index bdl)))))
    (setf (bdl-next bdl) new-bdl)
    (setf (bdl-prev next) new-bdl)
    new-bdl))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; delete-bdl

(defun delete-bdl (bdl)
  (let ((prev (bdl-prev bdl))
        (next (bdl-next bdl)))
    (unless (zerop (bdl-index next))
      (iterate-to-end next #'(lambda (bdl) (decf (bdl-index bdl)))))
    (setf (bdl-next prev) next)
    (setf (bdl-prev next) prev)
    t))
