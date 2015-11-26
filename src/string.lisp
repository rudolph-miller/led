(in-package :cl-user)
(defpackage led.string
  (:use :cl)
  (:import-from :led.line
                :*max-line-width*
                :string-to-line)
  (:export :string-to-lines))
(in-package :led.string)

(defun fold-string (string)
  (loop with pos = 0
        for fold-p = (and *max-line-width* (> (- (length string) pos) (1- *max-line-width*)))
        for content = (if fold-p
                          (let ((end (+ pos (- *max-line-width* 1))))
                            (prog1 (concatenate 'string (subseq string pos end) (list #\\))
                              (setq pos end)))
                          (subseq string pos))
        collecting content
        while fold-p))

(defun string-to-lines (string)
  (loop with stream = (make-string-input-stream string)
        for line = (read-line stream nil nil)
        while line
        nconc (mapcar #'string-to-line (fold-string line))))
