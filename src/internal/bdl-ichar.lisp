(in-package :cl-user)
(defpackage led.internal.bdl-ichar
  (:use :cl
        :led.internal.bidirectional-link
        :led.internal.character))
(in-package :led.internal.bdl-ichar)

(defstruct (bdl-ichar (:include bdl)
                      (:constructor %make-bdl-ichar)))
