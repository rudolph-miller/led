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
                :refresh-window
                :finalize)
  (:import-from :led.line
                :make-line))
(in-package :led.window)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; global parameters

(defparameter *window* nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; window

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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; initialize functions

(defun initialize-window-lines (window)
  (let ((lines (make-array (window-height window))))
    (setf (window-lines window) lines)))

(defun initialize-window-dimensions (window)
  (assert (typep window 'curses-window))
  (multiple-value-bind (width height) (window-dimensions *curses-window*)
    (setf (window-width window) width
          (window-height window) height)))

(defmethod initialize-instance :after ((window debug-window) &rest initargs)
  (declare (ignore initargs))
  (initialize-window-lines window))

(defparameter *curses-window* nil)

(defmethod initialize-instance :after ((window curses-window) &rest initargs)
  (declare (ignore initargs))
  (initialize)
  (setq *curses-window* (standard-window))
  (disable-echoing)
  (enable-raw-input :interpret-control-characters t)
  (enable-non-blocking-mode *curses-window*)
  (initialize-window-dimensions window)
  (initialize-window-lines window))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; macros

(defmacro with-curses-window (options &body body)
  (declare (ignore options))
  `(unwind-protect
        (let ((*window* (make-instance 'curses-window)))
          ,@body)
     (finalize)))


(defmacro with-debug-window (options &body body)
  (let ((width (getf options :width 50))
        (height (getf options :height 50)))
    `(let ((*window* (make-instance 'debug-window :width ,width :height ,height)))
       ,@body)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; refresh

(defun refresh (&optional (window *window*))
  (%refresh window))

(defgeneric %refresh (window))

(defmethod %refresh ((window curses-window))
  (move-cursor *curses-window* (window-x window) (window-y window))
  (refresh-window *curses-window*))

(defmethod %refresh ((window debug-window)))
