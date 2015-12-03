(in-package :cl-user)
(defpackage led.mapping.default
  (:use :cl
        :led.internal
        :led.buffer
        :led.mapping.util)
  (:export))
(in-package :led.mapping.default)

(global-set-key :normal "k" 'cursor-up)
(global-set-key :normal "j" 'cursor-down)
(global-set-key :normal "h" 'cursor-left)
(global-set-key :normal "l" 'cursor-right)
