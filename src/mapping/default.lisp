(in-package :cl-user)
(defpackage led.mapping.default
  (:use :cl
        :led.internal
        :led.buffer
        :led.mapping.util)
  (:export))
(in-package :led.mapping.default)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; cursor

(global-set-key :normal "k" 'cursor-up)
(global-set-key :normal "j" 'cursor-down)
(global-set-key :normal "h" 'cursor-left)
(global-set-key :normal "l" 'cursor-right)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; mode

(global-set-key :normal "i" 'insert-mode)
(global-set-key :normal "a" 'insert-mode-and-cursor-right)
(global-set-key :insert "<Esc>" 'normal-mode-and-cursor-left)
(global-set-key :insert "<C-j>" 'normal-mode-and-cursor-left)
(global-set-key :command-line "<Esc>" 'normal-mode)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; insert-char

(defun make-insert-ichar-fn (char)
  (lambda ()
    (insert-ichar (character-to-ichar char))
    (cursor-right)))

(loop for code from (char-code #\!) to (char-code #\~)
      for char = (code-char code)
      do (global-set-key :insert (string char) (make-insert-ichar-fn char)))
