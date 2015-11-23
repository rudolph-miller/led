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
   (contents :accessor window-contents
             :initform "")))

(defclass curses-window (window) ())
(defclass debug-window (window) ())

(defmethod initialize-instance :after ((window curses-window) &rest initargs)
  (declare (ignore initargs))
  (disable-echoing)
  (enable-raw-input :interpret-control-characters t)
  (enable-non-blocking-mode *standard-window*))

(defgeneric refresh (window))

(defmethod refresh ((window curses-window))
  (write-string-at-point *standard-window* (window-contents window) 0 0)
  (refresh-window *standard-window*))

(defmethod refresh ((window debug-window))
  (print (window-contents window)))
