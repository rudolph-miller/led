(in-package :cl-user)
(defpackage led.mapping.default
  (:use :cl
        :led.internal
        :led.window
        :led.buffer
        :led.mapping.util)
 (:export))
(in-package :led.mapping.default)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; globals

(defvar *character-loop-functions* nil)

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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; insert-char

(defun make-insert-char-fn (char)
  (lambda ()
    (insert-ichar (character-to-ichar char))
    (cursor-right)))

(push (lambda (char dsl)
        (global-set-key '(:insert :command-line) dsl (make-insert-char-fn char)))
      *character-loop-functions*)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; replace-char

(defun replace-char ()
  (let ((char (get-char)))
    (replace-ichar (character-to-ichar char))))

(global-set-key :normal "r" 'replace-char)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; insert-new-line

(defun insert-next-line-and-cursor-down-and-insert-mode ()
  (insert-next-line)
  (cursor-down)
  (insert-mode))

(defun insert-new-line-and-insert-mode ()
  (insert-new-line)
  (insert-mode))

(global-set-key :normal "o" 'insert-next-line-and-cursor-down-and-insert-mode)
(global-set-key :normal "O" 'insert-new-line-and-insert-mode)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; insert-eol

(global-set-key :insert "<CR>" 'insert-eol)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; delete

(global-set-key :normal "dd" 'delete-line)
(global-set-key :normal "x" 'delete-ichar)
(global-set-key :normal "<DEL>" 'delete-ichar)
(global-set-key '(:command-line :insert) "<C-h>" 'delete-prev-ichar)
(global-set-key '(:command-line :insert) "<BS>" 'delete-prev-ichar)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; command-line

(global-set-key :normal ":" 'command-line-mode)
(global-set-key :command-line "<Esc>" 'exit-command-line-mode)
(global-set-key :command-line "<CR>" 'exec-command)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; set characters loop

(dolist (fn *character-loop-functions*)
  (loop for code from (char-code #\Space) to (char-code #\~)
        for char = (code-char code)
        for dsl = (char-dsl char)
        do (funcall fn char dsl)))
