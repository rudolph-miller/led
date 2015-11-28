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
                :string-to-line
                :insert-ichar-to-line
                :insert-and-pop-last-ichar-to-line)
  (:export :string-to-lines
           :lines-to-string
           :lines-string-pos-ichar
           :insert-new-line-to-lines
           :insert-ichar-to-lines))
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

(defun line-chars-length (line)
  (let ((chars-len (length (line-chars line))))
    (if (line-eol-p line)
        (1+ chars-len)
        chars-len)))

(defun lines-chars-length (lines)
  (reduce #'+ (loop for line across lines collecting (line-chars-length line))))

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
  (let ((line (aref lines y)))
    (if (< (line-length line)
           (1- *max-line-width*))
        (insert-ichar-to-line ichar x line)
        (loop with lines-length = (length lines)
              for poped = (insert-and-pop-last-ichar ichar x line)
                then (insert-and-pop-last-ichar poped 0 next-line)
              for y from (1+ y)
              for next-line = (if (< y lines-length)
                                  (aref lines y)
                                  (progn (setf (line-eol-p next-line) nil)
                                         (setq lines (insert-new-line-to-lines
                                                      lines-length
                                                      lines))
                                         (aref lines y)))
              until (< (line-length next-line)
                       *max-line-width*)
              finally (insert-ichar poped 0 next-line)))
    lines))
