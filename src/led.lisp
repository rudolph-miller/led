(in-package :cl-user)
(defpackage led
  (:use :cl
        :charms))
(in-package :led)

(defvar *width* nil)
(defvar *height* nil)

(defun normalize-pointer (x y)
  (values (mod x *width*)
          (mod y *height*)))

(defun main ()
  (with-curses ()
    (enable-non-blocking-mode *standard-window*)
    (loop with x = 0
          with y = 0
          initially (multiple-value-bind (width height)
                        (window-dimensions *standard-window*)
                      (setq *width* width)
                      (setq *height* height))
          do (incf x)
             (incf y)
             (multiple-value-bind (normalized-x normalized-y) (normalize-pointer x y)
               (setq x normalized-x)
               (setq y normalized-y))
             (move-cursor *standard-window* x y)
             (refresh-window *standard-window*)
             (sleep 0.5))))
