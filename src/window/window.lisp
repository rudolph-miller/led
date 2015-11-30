(in-package :cl-user)
(defpackage :led.window.window
  (:use :cl
        :led.internal)
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
  (:export :*window*
           :make-window
           :window-width
           :window-height
           :window-x
           :window-y
           :window-lines
           :redraw
           :close-window))
(in-package :led.window.window)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; global parameters

(defvar *window* nil)


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
  (let ((lines (make-array (list (window-height window)
                                 (window-width window))
                           :initial-element nil)))
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

(defun window-write-lines (window)
  (loop with lines = (window-lines window)
        with height = (array-dimension lines 0)
        with width = (array-dimension lines 1)
        for y from 0 below height
        do (loop for x from 0 below width
                 for ichar = (aref lines y x)
                 do (when ichar (window-write-ichar window ichar x y)))))


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
;; close

(defun close-window ()
  (finalize)
  (setq *window* nil))
