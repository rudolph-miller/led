(in-package :cl-user)
(defpackage :led.window
  (:use :led.window.window)
  (:export ;; window
           :*window*
           :make-window
           :window-width
           :window-height
           :window-x
           :window-y
           :window-lines
           :window-entity
           :redraw
           :close-window))
