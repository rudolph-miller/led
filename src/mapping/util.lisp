(in-package :cl-user)
(defpackage led.mapping.util
  (:use :cl
        :led.internal)
  (:export :global-set-key
           :define-key))
(in-package :led.mapping.util)

(defun global-set-key (dsl function)
  (register-key dsl function *global-key-mapping* t))

(defun define-key (mapping dsl function)
  (register-key dsl function mapping t))
