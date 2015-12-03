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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; insert-char

(defun make-insert-char-fn (char)
  (lambda ()
    (insert-ichar (character-to-ichar char))
    (cursor-right)))

(defmacro set-insert-char-key (char)
  `(global-set-key :insert (string ,char) (make-insert-char-fn ,char)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; replace-char

(defun make-replace-char-fn (char)
  (lambda ()
    (replace-ichar (character-to-ichar char))))

(defmacro set-replace-char-key (char)
  `(global-set-key :normal (format nil "r~a" ,char) (make-replace-char-fn ,char)))


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
(global-set-key :insert "<C-h>" 'delete-prev-ichar)
(global-set-key :insert "<BS>" 'delete-prev-ichar)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; command-line

(global-set-key :normal ":" 'command-line-mode)
(global-set-key :command-line "<Esc>" 'exit-command-line-mode)
(global-set-key :command-line "<CR>" 'exec-current-command)

(defun make-insert-char-to-command-line-fn (char)
  (lambda ()
    (append-char-to-current-command char)
    (cursor-right)))

(defmacro set-insert-char-to-command-line-key (char)
  `(global-set-key :command-line
                   (string ,char)
                   (make-insert-char-to-command-line-fn ,char)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; set characters loop

(loop for code from (char-code #\!) to (char-code #\~)
      for char = (code-char code)
      do (set-insert-char-key char)
         (set-replace-char-key char)
         (set-insert-char-to-command-line-key char))
