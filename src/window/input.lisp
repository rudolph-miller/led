(in-package :cl-user)
(defpackage :led.window.input
  (:use :cl
        :led.window.window)
  (:import-from :charms/ll
                :wgetch)
  (:import-from :charms
                :window-pointer)
  (:import-from :babel
                :octets-to-string)
  (:export :get-char))
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
