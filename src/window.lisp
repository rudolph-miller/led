(in-package :cl-user)
(defpackage :led.window
  (:use :cl
        :charms))
(in-package :led.window)

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

(defun initialize-window-lines (window)
  (let ((lines (make-array (window-height window) :initial-element (make-window-line))))
    (setf (window-lines window) lines)))

(defun initialize-window-dimensions (window)
  (assert (typep window 'curses-window))
  (multiple-value-bind (width height) (window-dimensions *standard-window*)
    (setf (window-width window) width
          (window-height window) height)))

(defmethod initialize-instance :after ((window debug-window) &rest initargs)
  (declare (ignore initargs))
  (initialize-window-lines window))

(defmethod initialize-instance :after ((window curses-window) &rest initargs)
  (declare (ignore initargs))
  (disable-echoing)
  (enable-raw-input :interpret-control-characters t)
  (enable-non-blocking-mode *standard-window*)
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
                            (write-string-at-point *standard-window* content 0 index)))
  (refresh-window *standard-window*))

(defmethod refresh ((window debug-window))
  (iterate-window-lines window
                        #'(lambda (content index)
                            (declare (ignore index))
                            (print content))))
