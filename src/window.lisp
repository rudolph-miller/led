(in-package :cl-user)
(defpackage :led.window
  (:use :cl)
  (:import-from :charms
                :initialize
                :standard-window
                :disable-echoing
                :enable-raw-input
                :enable-non-blocking-mode
                :window-dimensions
                :write-string-at-point
                :move-cursor
                :refresh-window))
(in-package :led.window)

(defparameter *window* nil)

(defclass window ()
  ((width :accessor window-width
          :initarg :width)
   (height :accessor window-height
           :initarg :height)
   (x :accessor window-x
      :initform 0)
   (y :accessor window-y
      :initform 0)
   (lines :accessor window-lines
          :type vector)))

(defclass curses-window (window) ())
(defclass debug-window (window) ())

(defstruct window-line
  (content ""))

(defun set-line-content (window index content)
  (assert (<= (length content) (window-width window)))
  (let ((line (elt (window-lines window) index)))
    (setf (window-line-content line) content)))

(defun initialize-window-lines (window)
  (let ((lines (loop repeat (window-height window)
                     collecting (make-window-line) into result
                     finally (return (apply #'vector result)))))
    (setf (window-lines window) lines)))

(defun initialize-window-dimensions (window)
  (assert (typep window 'curses-window))
  (multiple-value-bind (width height) (window-dimensions *window*)
    (setf (window-width window) width
          (window-height window) height)))

(defmethod initialize-instance :after ((window debug-window) &rest initargs)
  (declare (ignore initargs))
  (initialize-window-lines window))

(defmethod initialize-instance :after ((window curses-window) &rest initargs)
  (declare (ignore initargs))
  (initialize)
  (setq *window* (standard-window))
  (disable-echoing)
  (enable-raw-input :interpret-control-characters t)
  (enable-non-blocking-mode *window*)
  (initialize-window-dimensions window)
  (initialize-window-lines window))

(defun iterate-window-lines (window function)
  (loop for line across (window-lines window)
        for i from 0
        do (funcall function (window-line-content line) i)))

(defgeneric refresh (window))

(defmethod refresh ((window curses-window))
  (iterate-window-lines window
                        #'(lambda (content index)
                            (write-string-at-point *window* content 0 index)))
  (move-cursor *window* (window-x window) (window-y window))
  (refresh-window *window*))

(defmethod refresh ((window debug-window))
  (iterate-window-lines window
                        #'(lambda (content index)
                            (declare (ignore index))
                            (print content))))
