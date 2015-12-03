(in-package :cl-user)
(defpackage led.mapping.util
  (:use :cl
        :led.internal)
  (:export :global-set-key))
(in-package :led.mapping.util)

(defun global-set-key (mode dsl function)
  (register-key mode dsl function *global-key-mapping* t)
  function)
