(in-package :cl-user)
(defpackage led.internal.key
  (:use :cl))
(in-package :led.internal.key)

(defparameter *global-key-mapping* (make-hash-table :test #'equal))

(defparameter *char-dsl* nil)

(defun char-dsl (char)
  (cdr (assoc char *code-dsl*)))

(defun dsl-char (dsl)
  (loop for (c . d) in *code-dsl*
        when (equal dsl d)
          do (return c)))

(defun register-dsl (code dsl)
  (push (cons (code-char code) dsl) *char-dsl*))

(loop for code from (char-code #\!) to (char-code #\~)
      do (register-dsl code (string (code-char code))))

(register-dsl 0 "<C-@>")
(register-dsl 1 "<C-a>")
(register-dsl 2 "<C-b>")
(register-dsl 3 "<C-c>")
(register-dsl 4 "<C-d>")
(register-dsl 5 "<C-e>")
(register-dsl 6 "<C-f>")
(register-dsl 7 "<C-g>")
(register-dsl 8 "<C-h>")
(register-dsl 9 "<C-i>")
(register-dsl 10 "<C-j>")
(register-dsl 11 "<C-k>")
(register-dsl 12 "<C-l>")
(register-dsl 13 "<C-m>")
(register-dsl 13 "<CR>")
(register-dsl 14 "<C-n>")
(register-dsl 15 "<C-o>")
(register-dsl 16 "<C-p>")
(register-dsl 17 "<C-q>")
(register-dsl 18 "<C-r>")
(register-dsl 19 "<C-s>")
(register-dsl 20 "<C-t>")
(register-dsl 21 "<C-u>")
(register-dsl 22 "<C-v>")
(register-dsl 23 "<C-w>")
(register-dsl 24 "<C-x>")
(register-dsl 25 "<C-y>")
(register-dsl 26 "<C-z>")
(register-dsl 27 "<Esc>")
(register-dsl 127 "[del]")

(defun parse-dsl (string)
  (loop with length = (length string)
        with position = 0
        for dsl-char = (loop for len from (- length position) downto 1
                             for dsl = (subseq string position (+ position len))
                             for char = (dsl-char dsl)
                             when char
                               do (incf position len)
                                  (return char))
        unless dsl-char
          do (error "Parse key binding error")
        collecting dsl-char
        while (< position length)))

(defun register-key-binding (string function))
