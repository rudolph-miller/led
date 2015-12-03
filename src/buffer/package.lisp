(in-package :cl-user)
(defpackage :led.buffer
  (:use :led.buffer.buffer
        :led.buffer.file-buffer
        :led.buffer.command-line-buffer)
  (:export ;; buffer
           :*current-buffer*
           :*buffers*
           :buffer
           :buffer-status
           :buffer-lines
           :redraw-buffer
           :prev-line
           :next-line
           :cursor-up
           :cursor-down
           :cursor-left
           :cursor-right
           :delete-line-at-point
           :delete-line
           :delete-ichar-at-point
           :delete-ichar
           :delete-prev-ichar
           :replace-ichar-at-point
           :replace-ichar
           :insert-new-line-at-point
           :insert-new-line
           :insert-next-line
           :insert-ichar-at-point
           :insert-ichar
           :insert-eol-at-point
           :insert-eol

           ;; file-buffer
           :file-buffer
           :write-buffer-to-file

           ;; command-line-buffer
           :*command-line-buffer*
           :*command-line-buffer-height*
           :command-line-buffer
           :on-command-line))
