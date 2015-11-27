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

(defstruct (window (:constructor %make-window))
  width
  height
  (x 0)
  (y 0)
  (lines #() :type array)
  entity)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; make-window

(defun initialize-window-lines (window)
  (setq *max-line-width* (window-width window))
  (let ((lines (make-vector-with (window-height window) #'make-line)))
    (setf (window-lines window) lines)))

(defun initialize-window-dimensions (window)
  (check-type window window)
  (multiple-value-bind (width height) (window-dimensions (window-entity window))
    (setf (window-width window) width
          (window-height window) height)))

(defun make-window ()
  (if *window*
      *window*
      (let ((window (%make-window)))
        (initialize)
        (setf (window-entity window) (standard-window))
        (disable-echoing)
        (enable-raw-input :interpret-control-characters t)
        (enable-non-blocking-mode (window-entity window))
        (initialize-window-dimensions window)
        (initialize-window-lines window)
        (setq *window* window))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; erase

(defun erase-window (window)
  (clear-window (window-entity window)))


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

(defun window-write-ichar (window ichar x y)
  (ignore-right-bottom-error window x y
    (if (ichar-attr ichar)
        (progn (wattron (window-entity window) (ichar-attr ichar))
               (write-char-at-point (window-entity window) (ichar-val ichar) x y)
               (wattroff (window-entity window) (ichar-attr ichar)))
        (write-char-at-point (window-entity window) (ichar-val ichar) x y))))

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

(defun update-cursor (window)
  (move-cursor (window-entity window) (window-x window) (window-y window)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; refresh

(defun refresh (window)
  (refresh-window (window-entity window)))


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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; util

(defun get-window-line (y &optional (window *window*))
  (aref (window-lines window) y))

(defun set-window-line (line y &optional (window *window*))
  (setf (aref (window-lines window) y) line))
