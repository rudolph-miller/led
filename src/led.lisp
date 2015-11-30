(in-package :cl-user)
(defpackage led
  (:use :cl)
  (:import-from :led.window
   :make-window)
  (:import-from :led.buffer.buffer
                :*current-buffer*
                :*buffers*
                :redraw-buffer
                :prev-line
                :next-line
                :cursor-up
                :cursor-down
                :cursor-left
                :cursor-right
                :delete-line
                :delete-ichar
                :insert-new-line-at-point
                :insert-ichar-at-point
                :insert-new-line
                :insert-ichar)
  (:import-from :led.buffer.file-buffer
                :file-buffer
                :write-buffer-to-file))
(in-package :led)
