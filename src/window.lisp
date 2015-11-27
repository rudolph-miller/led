(in-package :cl-user)
(defpackage :led.window
  (:use :cl)
  (:import-from :charms/ll
                :wattron
                :wattroff)
  (:import-from :charms
                :initialize
                :standard-window
                :disable-echoing
                :enable-raw-input
                :enable-non-blocking-mode
                :window-dimensions
                :write-char-at-point
                :move-cursor
                :refresh-window
                :clear-window
                :finalize)
  (:import-from :led.util
                :make-vector-with)
  (:import-from :led.character
                :make-ichar
                :ichar-val
                :ichar-attr)
  (:import-from :led.line
                :*max-line-width*
                :make-line
                :line-chars)
  (:export :get-window-line
           :set-window-line))
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
          :type vector)
   (entity :accessor window-entity)))

(defclass curses-window (window) ())
(defclass debug-window (window) ())


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; initialize functions

(defun initialize-window-lines (window)
  (setq *max-line-width* (window-width window))
  (let ((lines (make-vector-with (window-height window) #'make-line)))
    (setf (window-lines window) lines)))

(defun initialize-window-dimensions (window)
  (assert (typep window 'curses-window))
  (multiple-value-bind (width height) (window-dimensions (window-entity window))
    (setf (window-width window) width
          (window-height window) height)))

(defmethod initialize-instance :after ((window debug-window) &rest initargs)
  (declare (ignore initargs))
  (initialize-window-lines window)
  (setf (window-entity window) (make-array (list (window-width window) (window-height window)) :initial-element nil)))

(defmethod initialize-instance :after ((window curses-window) &rest initargs)
  (declare (ignore initargs))
  (initialize)
  (setf (window-entity window) (standard-window))
  (disable-echoing)
  (enable-raw-input :interpret-control-characters t)
  (enable-non-blocking-mode (window-entity window))
  (initialize-window-dimensions window)
  (initialize-window-lines window))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; erase

(defgeneric erase-window (window))

(defmethod erase-window ((window curses-window))
  (clear-window (window-entity window)))

(defmethod erase-window ((window debug-window))
  (declare (ignore window)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; write

(defmacro ignore-right-bottom-error (window x y &body body)
  `(if (and (= ,x (1- (window-width ,window)))
            (= ,y (1- (window-height ,window))))
       (handler-case
           (progn ,@body)
         (error (e)
           (unless (equal (princ-to-string e) "Error in curses call.")
             (error e))))
       (progn ,@body)))

(defgeneric window-write-ichar (window ichar x y))

(defmethod window-write-ichar ((window curses-window) ichar x y)
  (ignore-right-bottom-error window x y
    (if (ichar-attr ichar)
        (progn (wattron (window-entity window) (ichar-attr ichar))
               (write-char-at-point (window-entity window) (ichar-val ichar) x y)
               (wattroff (window-entity window) (ichar-attr ichar)))
        (write-char-at-point (window-entity window) (ichar-val ichar) x y))))

(defmethod window-write-ichar ((window debug-window) ichar x y)
  (setf (aref (window-entity window) x y) (ichar-val ichar)))

(defun window-write-line (window line y)
  (loop for ichar across (line-chars line)
        for index from 0
        do (window-write-ichar window ichar index y)))

(defun window-write-lines (window)
  (loop for line across (window-lines window)
        for index from 0
        do (window-write-line window line index)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; update

(defgeneric update-cursor (window))

(defmethod update-cursor ((window curses-window))
  (move-cursor (window-entity window) (window-x window) (window-y window)))


(defmethod update-cursor ((window debug-window))
  (setf (aref (window-entity window) (window-x window) (window-y window)) #\*))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; refresh

(defgeneric refresh (window))

(defmethod refresh ((window curses-window))
  (refresh-window (window-entity window)))

(defmethod refresh ((window debug-window))
  (flet ((write-width-line ()
           (format t (format nil "~~~a{-~~}~~%" (+ (window-width window) 2)) :dummy)))
    (loop with entity = (window-entity window)
          for y from 0 below (array-dimension entity 1)
            initially (write-width-line)
          do (loop for x from 0 below (array-dimension entity 0)
                   for char = (aref entity x y)
                     initially (write-char #\|)
                   if char
                     do (write-char char)
                   else
                     do (write-char #\Space)
                   finally (progn (write-char #\|)
                                  (fresh-line)))
          finally (write-width-line))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; redraw

(defun redraw (&optional (window *window*))
  (erase-window window)
  (window-write-lines window)
  (update-cursor window)
  (refresh window))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; macros

(defmacro with-curses-window (options &body body)
  (declare (ignore options))
  `(unwind-protect
        (let ((*window* (make-instance 'curses-window)))
          ,@body)
     (finalize)))


(defmacro with-debug-window (options &body body)
  (let ((width (getf options :width 100))
        (height (getf options :height 30)))
    `(let ((*window* (make-instance 'debug-window :width ,width :height ,height)))
       ,@body)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; util

(defun get-window-line (y &optional (window *window*))
  (aref (window-lines window) y))

(defun set-window-line (line y &optional (window *window*))
  (setf (aref (window-lines window) y) line))
