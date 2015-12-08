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
;; bdl

(defstruct bdl
  prev
  next
  value
  index
  top-p)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; top-bdl

(defun make-top-bdl ()
  (let ((bdl (make-bdl :top-p t
                       :index 0)))
    (setf (bdl-prev bdl) bdl)
    (setf (bdl-next bdl) bdl)
    bdl))

(defun top-bdl-p (bdl)
  (bdl-top-p bdl))


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
  (let ((next (bdl-next bdl)))
    (setf (bdl-index new-bdl) (if (top-bdl-p bdl)
                                  0
                                  (1+ (bdl-index bdl))))
    (setf (bdl-prev new-bdl) bdl)
    (setf (bdl-next new-bdl) next)
    (unless (top-bdl-p next)
      (iterate-bdl next #'(lambda (bdl) (incf (bdl-index bdl)))))
    (setf (bdl-next bdl) new-bdl)
    (setf (bdl-prev next) new-bdl)
    new-bdl))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; replace-bdl

(defun replace-bdl (new-bdl bdl)
  (assert (not (top-bdl-p bdl)))
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

