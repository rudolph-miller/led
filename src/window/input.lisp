(in-package :cl-user)
(defpackage :led.window.input
  (:use :cl
        :led.internal
        :led.window.window)
  (:import-from :charms/ll
                :wgetch)
  (:import-from :charms
                :window-pointer)
  (:import-from :babel
                :octets-to-string)
  (:import-from :bordeaux-threads
                :make-thread
                :destroy-thread)
  (:export :start-input-loop
           :stop-input-loop))
(in-package :led.window.input)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; get-char

(defun get-code ()
  (loop for code = (wgetch (window-pointer (window-entity *window*)))
        while (= code -1)
        finally (return code)))

(defun code-length (code)
  (cond
    ((<= code #x7f) 1)
    ((<= #xc2 code #xdf) 2)
    ((<= #xe0 code #xef) 3)
    ((<= #xf0 code #xf4) 4)
    (t 1)))

(defun octets-to-character (octets)
  (aref (octets-to-string octets) 0))

(defun get-char ()
  (loop with first-code = (get-code)
        repeat (1- (code-length first-code))
        for code = (get-code)
        collecting code into codes
        finally (return
                  (if codes
                      (octets-to-character
                       (concatenate '(vector (unsigned-byte 8))
                                    (list first-code)
                                    codes))
                      (code-char first-code)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; input-loop

(defun fn-or-next-contexts (char contexts)
  (loop for context in contexts
        for got = (gethash char context)
        when (typep got 'function)
          do (return got)
        when got
          collecting got))

(defun input-loop ()
  (loop for char = (get-char)
        with contexts = (list *global-key-mapping*)
        for got = (fn-or-next-contexts char contexts)
        do (etypecase got
             (cons (setq contexts got))
             (function (funcall got))
             (null (setq contexts (list *global-key-mapping*))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; start-input-loop and stop-input-loop

(defvar *input-loop-thread* nil)

(defun start-input-loop ()
  (setq *input-loop-thread* (make-thread #'input-loop)))

(defun stop-input-loop ()
  (destroy-thread *input-loop-thread*)
  (setq *input-loop-thead* nil)
  t)
