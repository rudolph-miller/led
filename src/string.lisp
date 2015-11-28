(in-package :cl-user)
(defpackage led.string
  (:use :cl)
  (:import-from :led.character
                :ichar-val)
  (:import-from :led.line
                :*max-line-width*
                :make-line
                :line-chars
                :line-eol-p
                :line-length
                :ichars-to-line
                :string-to-line
                :insert-ichar-to-line
                :insert-and-pop-last-ichar-to-line)
  (:export :string-to-lines
           :ichars-to-lines
           :lines-to-string
           :lines-to-ichars
           :lines-string-pos-ichar
           :insert-new-line-to-lines
           :insert-ichar-to-lines))
(in-package :led.string)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ichars-to-lines

(defun fold (sequence)
  (loop with pos = 0
        with seq-len = (length sequence)
        for fold-p = (and *max-line-width*
                          (> (- seq-len pos)
                             *max-line-width*))
        for content = (if fold-p
                          (let ((end (+ pos *max-line-width*)))
                            (prog1 (subseq sequence pos end)
                              (setq pos end)))
                          (subseq sequence pos))
        collecting content
        while fold-p))

(defun ichars-to-lines (ichars)
  (loop for ichar across ichars
        unless ichar
          nconc (let ((lines (mapcar #'ichars-to-line
                                     (mapcar #'(lambda (folded)
                                                 (apply #'vector folded))
                                             (fold tmp)))))
                  (setf (line-eol-p (car (last lines))) t)
                  lines) into result
        collecting ichar into tmp
        finally (return (apply #'vector result))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; string-to-lines

(defun string-to-lines (string)
  (loop with stream = (make-string-input-stream string)
        for line = (read-line stream nil nil)
        while line
        for lines = (mapcar #'string-to-line (fold line))
        for last-line = (car (last lines))
        do (setf (line-eol-p last-line) t)
        nconc lines into result
        finally (return (apply #'vector result))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; lines-to-ichars

(defun line-chars-length (line)
  (let ((chars-len (length (line-chars line))))
    (if (line-eol-p line)
        (1+ chars-len)
        chars-len)))

(defun lines-chars-length (lines)
  (reduce #'+ (loop for line across lines collecting (line-chars-length line))))

(defun lines-to-ichars (lines)
  (loop with result = (make-array (lines-chars-length lines))
        with pos = 0
        for line across lines
        do (loop for ichar across (line-chars line)
                 do (setf (elt result pos) ichar)
                    (incf pos)
                 finally (when (line-eol-p line)
                           (setf (elt result pos) nil)
                           (incf pos)))
        finally (return result)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; lines-to-string

(defun lines-to-string (lines)
  (loop with result = (make-string (lines-chars-length lines))
        with pos = 0
        for line across lines
        do (loop for ichar across (line-chars line)
                 do (setf (elt result pos) (ichar-val ichar))
                    (incf pos)
                 finally (when (line-eol-p line)
                           (setf (elt result pos) #\NewLine)
                           (incf pos)))
        finally (return result)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; lines-string-pos-ichar

(defun line-string-pos-ichar (x lines)
  (loop for line across lines
        do (cond
             ((< x (length (line-chars line)))
              (return (aref (line-chars line) x)))
             (t
              (decf x (length (line-chars line)))))
        finally (return nil)))

(defun lines-string-pos-ichar (x y lines)
  (loop for line across lines
        for index from 0
        when (= y 0)
          do (return (line-string-pos-ichar x (subseq lines index)))
        when (line-eol-p line)
          do (decf y)
        finally (return nil)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; insert

(defun insert-new-line-to-lines (y lines)
  (let ((result (make-array (1+ (length lines)))))
    (setf (subseq result 0 y) (subseq lines 0 y))
    (setf (aref result y) (make-line :eol-p t))
    (setf (subseq result (1+ y)) (subseq lines y))
    result))

(defun insert-ichar-to-lines (ichar x y lines)
  (let* ((end (loop for index from y
                    for line = (aref lines index)
                    when (line-eol-p line)
                      do (return index)))
         (ichars (lines-to-ichars (subseq lines y (1+ end))))
         (result-ichars (make-array (1+ (length ichars)))))
    (setf (subseq result-ichars 0 x) (subseq ichars 0 x))
    (setf (aref result-ichars x) ichar)
    (setf (subseq result-ichars (1+ x)) (subseq ichars x))
    (ichars-to-lines result-ichars)))
