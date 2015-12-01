(in-package :cl-user)
(defpackage led.mapping.default
  (:use :cl
        :led.internal
        :led.buffer
        :led.mapping.util)
  (:export))
(in-package :led.mapping.default)

(global-set-key "k" 'cursor-up)
(global-set-key "j" 'cursor-down)
(global-set-key "h" 'cursor-left)
(global-set-key "l" 'cursor-right)
