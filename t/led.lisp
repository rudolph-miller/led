(in-package :cl-user)
(defpackage led-test
  (:use :cl
        :led
        :prove))
(in-package :led-test)

;; NOTE: To run this test file, execute `(asdf:test-system :led)' in your Lisp.

(plan nil)

;; blah blah blah.

(finalize)
