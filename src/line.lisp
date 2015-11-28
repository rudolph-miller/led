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
           :ichars-to-line
           :string-to-line
           :migrate-line-to-line
           :line-chars-with-padding
           :append-ichar-to-line))
(in-package :led.line)

(defparameter *max-line-width* nil)

(defstruct (line (:constructor %make-line))
  (chars #() :type array)
  (eol-p nil :type boolean))

(defun line-length (line)
  (length (line-chars line)))

(defun make-line (&key (chars #()) (eol-p nil))
  (when *max-line-width* (assert (<= (length chars) *max-line-width*)))
  (%make-line :chars chars :eol-p eol-p))

(defun ichars-to-line (ichars)
  (make-line :chars ichars))

(defun string-to-line (string)
  (let ((chars (loop for character across string
                     collecting (character-to-ichar character) into result
                     finally (return (apply #'vector result)))))
    (ichars-to-line chars)))

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
      (make-line :chars result-chars))))

(defun line-chars-with-padding (line length)
  (assert (<= (line-length line) length))
  (loop with result = (make-array length :initial-element nil)
        with chars = (line-chars line)
        for ichar across chars
        for i from 0
        do (setf (aref result i) ichar)
           finally (return result)))

(defun append-ichar-to-line (ichar line)
  (let* ((chars (line-chars line))
         (result-chars (make-array (1+ (length chars)))))
    (setq result-chars (replace result-chars chars))
    (setf (aref result-chars (length chars)) ichar)
    (make-line :chars result-chars :eol-p (line-eol-p line))))
