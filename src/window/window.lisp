(in-package :cl-user)
(defpackage :led.window.window
  (:use :cl
        :led.internal)
  (:import-from :charms/ll
                :mvwdelch
                :wattron
                :wattroff
                :*escdelay*)
  (:import-from :charms
                :initialize
                :standard-window
                :disable-echoing
                :enable-raw-input
                :enable-non-blocking-mode
                :enable-extra-keys
                :window-dimensions
                :window-pointer
                :write-string-at-point
                :move-cursor
                :refresh-window
                :clear-window
                :finalize)
  (:export :*window*
           :*escape-delay*
           :make-window
           :set-window-ichar
           :window-width
           :window-height
           :window-x
           :window-y
           :window-board
           :window-entity
           :redraw
           :close-window))
(in-package :led.window.window)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; global parameters

(defvar *window* nil)

(defvar *escape-delay* 10)

(defvar *changed-points* nil)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; window

(defstruct (window (:constructor %make-window))
  width
  height
  (x 0)
  (y 0)
  (board #() :type array)
  entity)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; make-window

(defun initialize-window-board (window)
  (let ((lines (make-array (list (window-height window)
                                 (window-width window))
                           :initial-element nil)))
    (setf (window-board window) lines)))

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
        (enable-raw-input)
        (enable-non-blocking-mode (window-entity window))
        (enable-extra-keys (window-entity window))
        (initialize-window-dimensions window)
        (initialize-window-board window)
        (setq *escdelay* *escape-delay*)
        (setq *window* window))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; set-window-ichar

(defun set-window-ichar (x y ichar  &optional (window *window*))
  (prog1 (setf (aref (window-board window) y x) ichar)
    (push (cons x y) *changed-points*)))


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
               (write-string-at-point (window-entity window)
                                      (string (ichar-char ichar)) x y)
               (wattroff (window-entity window) (ichar-attr ichar)))
        (write-string-at-point (window-entity window)
                               (string (ichar-char ichar)) x y))))

(defun window-write-lines (window &optional force-update)
  (labels ((window-delete-ichar (window x y)
             (mvwdelch (window-pointer (window-entity window)) y x))
           (write-ichar (x y ichar)
             (if ichar
                 (window-write-ichar window ichar x y)
                 (window-delete-ichar window x y))))
    (if force-update
        (loop with lines = (window-board window)
              with height = (array-dimension lines 0)
              with width = (array-dimension lines 1)
              for y from 0 below height
              do (loop for x from 0 below width
                       for ichar = (aref lines y x)
                       do (write-ichar x y ichar)))
        (loop for (x . y) in *changed-points*
              for ichar = (aref (window-board window) y x)
              do (write-ichar x y ichar)))
    (setq *changed-points* nil)))


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

(defun redraw (&optional force-update (window *window*))
  (window-write-lines window force-update)
  (update-cursor window)
  (refresh window))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; close

(defun close-window ()
  (finalize)
  (setq *window* nil))
