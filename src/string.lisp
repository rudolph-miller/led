(in-package :cl-user)
(defpackage led.string
  (:use :cl)
  (:import-from :led.character
                :ichar-val)
  (:import-from :led.line
                :*max-line-width*
                :line-chars
                :line-eol-p
                :string-to-line)
  (:export :string-to-lines
           :lines-to-string
           :lines-string-pos-ichar))
(in-package :led.string)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; string-to-lines

(defun fold-string (string)
  (loop with pos = 0
        for fold-p = (and *max-line-width*
                          (> (- (length string) pos)
                             *max-line-width*))
        for content = (if fold-p
                          (let ((end (+ pos *max-line-width*)))
                            (prog1 (subseq string pos end)
                              (setq pos end)))
                          (subseq string pos))
        collecting content
        while fold-p))

(defun string-to-lines (string)
  (loop with stream = (make-string-input-stream string)
        for line = (read-line stream nil nil)
        while line
        for lines = (mapcar #'string-to-line (fold-string line))
        for last-line = (car (last lines))
        do (setf (line-eol-p last-line) t)
        nconc lines into result
        finally (return (apply #'vector result))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; lines-to-string

(defun line-length (line)
  (let ((chars-len (length (line-chars line))))
    (if (line-eol-p line)
        (1+ chars-len)
        chars-len)))

(defun lines-length (lines)
  (reduce #'+ (loop for line across lines collecting (line-length line))))

(defun lines-to-string (lines)
  (loop with result = (make-string (lines-length lines))
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
