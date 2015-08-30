#|
  This file is a part of led project.
  Copyright (c) 2015 Rudolph Miller (chopsticks.tk.ppfm@gmail.com)
|#

(in-package :cl-user)
(defpackage led-test-asd
  (:use :cl :asdf))
(in-package :led-test-asd)

(defsystem led-test
  :author "Rudolph Miller"
  :license "MIT"
  :homepage "https://github.com/Rudolph-Miller/led"
  :depends-on (:led
               :prove)
  :components ((:module "t"
                :components
                ((:test-file "led"))))
  :description "Test system for led."

  :defsystem-depends-on (:prove-asdf)
  :perform (test-op :after (op c)
                    (funcall (intern #.(string :run-test-system) :prove-asdf) c)
                    (asdf:clear-system c)))
