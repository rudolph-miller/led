(in-package :cl-user)
(defpackage led.internal.bdl-ichar
  (:use :cl
        :led.internal.bidirectional-link
        :led.internal.character)
  (:export :bdl-ichar
           :bdl-ichar-ichar
           :make-top-bdl-ichar
           :make-dbl-ichar
           :insert-prev-bdl-ichar
           :insert-next-bdl-ichar
           :replace-bdl-ichar
           :delete-bdl-ichar
           :bdl-ichar-length
           :iterate-bdl-ichars))
(in-package :led.internal.bdl-ichar)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; bdl-ichar

(defstruct (bdl-ichar (:include bdl)
                      (:constructor %make-bdl-ichar)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; bdl-ichar-ichar

(defun bdl-ichar-ichar (bdl-ichar)
  (bdl-value bdl-ichar))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; make-top-bdl-ichar

(defun make-top-bdl-ichar ()
  (make-top-bdl))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; make-bdl-ichar

(defun make-bdl-ichar (object)
  (let ((ichar (etypecase object
                 (character (character-to-ichar object))
                 (ichar object))))
    (%make-bdl-ichar :value ichar)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; insert-prev-bdl-ichar

(defun insert-prev-bdl-ichar (object bdl-ichar)
  (let ((new-bdl-ichar (if (typep object 'bdl-ichar)
                      object
                      (make-bdl-ichar object))))
    (insert-prev new-bdl-ichar bdl-ichar)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; insert-next-bdl-ichar

(defun insert-next-bdl-ichar (object bdl-ichar)
  (let ((new-bdl-ichar (if (typep object 'bdl-ichar)
                      object
                      (make-bdl-ichar object))))
    (insert-next new-bdl-ichar bdl-ichar)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; replacet-bdl-ichar

(defun replacet-bdl-ichar (object bdl-ichar)
  (let ((new-bdl-ichar (if (typep object 'bdl-ichar)
                      object
                      (make-bdl-ichar object))))
    (replace-bdl new-bdl-ichar bdl-ichar)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; delete-bdl-ichar

(defun delete-bdl-ichar (bdl-ichar)
  (delete-bdl bdl-ichar))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; bdl-ichar-length

(defun bdl-ichar-length (top-bdl-ichar)
  (bdl-length top-bdl-ichar))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; iterate-bdl-ichars

(defun iterate-bdl-ichars (top-bdl-ichar fn)
  (iterate-bdl top-bdl-ichar fn))
