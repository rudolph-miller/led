(in-package :cl-user)
(defpackage led.internal.bidirectional-link
  (:use :cl)
  (:export :bdl
           :make-bdl
           :make-top-bdl
           :top-bdl-p
           :make-dummy-bdl
           :dummy-bdl-p
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
           :iterate-bdl
           :get-by-index
           :get-first
           :get-last
           :insert-prev
           :insert-next
           :replace-bdl
           :delete-bdl))
(in-package :led.internal.bidirectional-link)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; globals

(defvar +top-index+ :top)

(defvar +dummy-value+ :dummy)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; bdl

(defstruct bdl
  prev
  next
  value
  index)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; top-bdl

(defun make-top-bdl ()
  (let ((bdl (make-bdl :index +top-index+)))
    (setf (bdl-prev bdl) bdl)
    (setf (bdl-next bdl) bdl)
    bdl))

(defun top-bdl-p (bdl)
  (eql (bdl-index bdl) +top-index+))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; dummy-bdl

(defun make-dummy-bdl (&key value)
  (make-bdl :index +dummy-value+ :value value))

(defun dummy-bdl-p (bdl)
  (eql (bdl-index bdl) +dummy-value+))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; bdl-legth

(defun bdl-length (bdl)
  (assert (top-bdl-p bdl))
  (let ((prev (bdl-prev bdl)))
    (if (or (top-bdl-p prev)
            (dummy-bdl-p prev))
        0
        (1+ (bdl-index prev)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; print-object

(defmethod print-object ((object bdl) stream)
  (cond
    ((top-bdl-p object)
     (print-unreadable-object (object stream :identity t)
       (format stream "TOP-BDL :LENGTH ~a" (bdl-length object))))
    ((dummy-bdl-p object)
     (print-unreadable-object (object stream :identity t)
       (format stream "DUMMY-BDL")))
    (t
      (print-unreadable-object (object stream :type t :identity t)
        (format stream ":VALUE ~a :INDEX ~a" (bdl-value object) (bdl-index object))))))


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

(defun iterate-bdl (bdl fn)
  (loop for item = (if (top-bdl-p bdl) (next bdl) bdl)
          then (next item)
        while item
        do (funcall fn item)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; get-by-index

(defun get-by-index (index top-bdl)
  (assert (top-bdl-p top-bdl))
  (iterate-bdl
   top-bdl
   (lambda (item)
     (when (= (bdl-index item) index)
       (return-from get-by-index item)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; get-first

(defun get-first (top-bdl)
  (assert (top-bdl-p top-bdl))
  (next top-bdl))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; get-last

(defun get-last (top-bdl)
  (assert (top-bdl-p top-bdl))
  (prev top-bdl))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; insert-prev

(defun insert-prev (new-bdl bdl)
  (assert (not (top-bdl-p bdl)))
  (assert (not (dummy-bdl-p bdl)))
  (assert (not (dummy-bdl-p new-bdl)))
  (let ((prev (bdl-prev bdl)))
    (setf (bdl-index new-bdl) (bdl-index bdl))
    (setf (bdl-prev new-bdl) (bdl-prev prev))
    (setf (bdl-next new-bdl) bdl)
    (setf (bdl-prev bdl) new-bdl)
    (setf (bdl-next prev) new-bdl)
    (iterate-bdl bdl #'(lambda (bdl) (incf (bdl-index bdl))))
    new-bdl))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; insert-next

(defun insert-next (new-bdl bdl)
  (assert (not (dummy-bdl-p bdl)))
  (assert (not (and (next bdl)
                    (dummy-bdl-p (next bdl)))))
  (let ((next (bdl-next bdl)))
    (unless (dummy-bdl-p new-bdl)
      (setf (bdl-index new-bdl) (if (top-bdl-p bdl)
                                    0
                                    (1+ (bdl-index bdl)))))
    (setf (bdl-prev new-bdl) bdl)
    (setf (bdl-next new-bdl) next)
    (unless (or (top-bdl-p next)
                (dummy-bdl-p new-bdl))
      (iterate-bdl next #'(lambda (bdl) (incf (bdl-index bdl)))))
    (setf (bdl-next bdl) new-bdl)
    (setf (bdl-prev next) new-bdl)
    new-bdl))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; replace-bdl

(defun replace-bdl (new-bdl bdl)
  (assert (not (top-bdl-p bdl)))
  (assert (not (dummy-bdl-p bdl)))
  (assert (not (dummy-bdl-p new-bdl)))
  (let ((prev (bdl-prev bdl))
        (next (bdl-next bdl)))
    (setf (bdl-prev new-bdl) prev)
    (setf (bdl-next new-bdl) next)
    (setf (bdl-index new-bdl) (bdl-index bdl))
    (setf (bdl-next prev) new-bdl)
    (setf (bdl-prev next) new-bdl)
    new-bdl))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; delete-bdl

(defun delete-bdl (bdl)
  (assert (not (top-bdl-p bdl)))
  (let ((prev (bdl-prev bdl))
        (next (bdl-next bdl)))
    (unless (zerop (bdl-index next))
      (iterate-bdl next #'(lambda (bdl) (decf (bdl-index bdl)))))
    (setf (bdl-next prev) next)
    (setf (bdl-prev next) prev)
    t))

