(in-package :cl-user)
(defpackage :led.window
  (:use :led.window.window
        :led.window.input)
  (:export ;; window
           :*window*
           :make-window
           :set-window-ichar
           :window-width
           :window-height
           :window-x
           :window-y
           :window-lines
           :window-entity
           :redraw
           :close-window

           ;; input
           :*stop-input-loop*
           :get-char
           :input-loop
           :start-input-loop
           :stop-input-loop))
