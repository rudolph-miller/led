(in-package :cl-user)
(defpackage led.line
  (:use :cl)
  (:import-from :led.util
                :make-vector-with)
  (:import-from :led.character
                :character-to-ichar)
  (:export :*max-line-width*
           :make-line
           :line-chars
           :line-eol-p
           :line-length
           :string-to-line
           :append-ichar-to-line
           :migrate-line-to-line
           :line-chars-with-padding
           :insert-icha
           :insert-and-pop-last-ichar))
(in-package :led.line)

(defparameter *max-line-width* nil)

(defstruct (line (:constructor %make-line))
  (chars #() :type array)
  (eol-p nil :type boolean))

(defun line-length (line)
  (length (line-chars line)))

(defun make-line (&optional (chars #()))
  (when *max-line-width* (assert (<= (length chars) *max-line-width*)))
  (%make-line :chars chars))

(defun string-to-line (string)
  (let ((chars (loop for character across string
                     collecting (character-to-ichar character) into result
                     finally (return (apply #'vector result)))))
    (make-line chars)))

(defun append-ichar-to-line (line ichar)
  (let* ((chars (line-chars line))
         (result-chars (make-array (1+ (length chars))))
         (result (make-line)))
    (setq result-chars (replace result-chars chars))
    (setf (aref result-chars (length chars)) ichar)
    (setf (line-chars result) result-chars)
    (setf (line-eol-p result) (line-eol-p line))
    result))

(defun migrate-line-to-line (line target start end)
  (flet ((make-empty-ichar-vector (len)
           (make-vector-with len #'(lambda () (character-to-ichar #\Space)))))
    (let* ((line-chars (line-chars line))
           (filled-line-chars (let ((array (make-empty-ichar-vector (- end start))))
                                (replace array line-chars)))
           (target-chars (line-chars target))
           (result-chars (make-empty-ichar-vector (max (+ (length line-chars) start)
                                                       (length target-chars)))))
      (setq result-chars (replace result-chars target-chars))
      (setq result-chars (replace result-chars filled-line-chars :start1 start))
      (make-line result-chars))))

(defun line-chars-with-padding (line length)
  (assert (<= (line-length line) length))
  (loop with result = (make-array length :initial-element nil)
        with chars = (line-chars line)
        for ichar across chars
        for i from 0
        do (setf (aref result i) ichar)
           finally (return result)))

(defun insert-ichar (ichar line x)
  (let* ((chars (line-chars line))
        (result-chars (make-array (1+ (length chars)))))
    (setf (subseq result-chars 0 x) (subseq chars 0 x))
    (setf (aref result-chars x) ichar)
    (setf (subseq result-chars (1+ x)) (subseq chars x))
    (setf (line-chars line) result-chars)))

(defun insert-and-pop-last-ichar (ichar line x)
  (let* ((chars (line-chars line))
         (last-ichar (aref chars (1- (length chars))))
         (result-chars (make-array (length chars))))
    (setf (subseq result-chars 0 x) (subseq chars 0 x))
    (setf (aref result-chars x) ichar)
    (setf (subseq result-chars (1+ x)) (subseq chars x (1- (length chars))))
    (setf (line-chars line) result-chars)
    last-ichar))
