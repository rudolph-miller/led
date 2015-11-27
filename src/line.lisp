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
           :string-to-line
           :append-ichar-to-line
           :migrate-line-to-line))
(in-package :led.line)

(defparameter *max-line-width* nil)

(defstruct (line (:constructor %make-line))
  (chars #() :type array)
  (eol-p nil :type boolean))

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
