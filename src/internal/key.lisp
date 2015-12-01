(in-package :cl-user)
(defpackage led.internal.key
  (:use :cl)
  (:export :*global-key-mapping*
           :get-namespace
           :create-namespace
           :register-key
           :unregister-key))
(in-package :led.internal.key)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; globals

(defparameter *global-key-mapping* (make-hash-table :test #'equal))

(defparameter *mapping-namespaces* (make-hash-table :test #'equal))

(defparameter *char-dsl* nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; char-dsl and dsl-char

(defun char-dsl (char)
  (cdr (assoc char *char-dsl*)))

(defun dsl-char (dsl)
  (loop for (c . d) in *char-dsl*
        when (equal dsl d)
          do (return c)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; register-dsl

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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; namespace

(defun get-namespace (name)
  (gethash name *mapping-namespaces*))

(defun create-namespace (name)
  (unless (get-namespace name)
  (setf (gethash name *mapping-namespaces*) (make-hash-table :test #'equal))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; register-key and unregister-key

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
          do (error "Parse key dsl error")
        collecting dsl-char
        while (< position length)))

(defun register-key (dsl target &optional (mapping *global-key-mapping*) (force nil))
  (let ((chars (parse-dsl dsl)))
    (loop for char in chars
          with last-position = (1- (length chars))
          for position from 0
          for last-p = (= position last-position)
          with prev = mapping
          for got = (gethash char prev)
          if last-p
            do (cond
                 ((and (not force) got)
                  (if (and (typep got 'hash-table)
                           (= (hash-table-count got) 0))
                      (setf (gethash char prev) target)
                      (error "Key Conflict")))
                 ((null target)
                  (remhash char prev))
                 ((typep target 'function)
                  (setf (gethash char prev) target))
                 ((typep target 'symbol)
                  (let ((function (symbol-function target)))
                    (setf (gethash char prev) function))))
          else
            do (setq prev
                     (etypecase got
                       (null (setf (gethash char prev) (make-hash-table :test #'equal)))
                       (function (if force
                                     (setf (gethash char prev)
                                           (make-hash-table :test #'equal))
                                     (error "Key Conflict")))
                       (hash-table got))))))

(defun unregister-key (dsl &optional (mapping *global-key-mapping*))
  (register-key dsl nil mapping t))
