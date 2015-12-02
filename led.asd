#|
  This file is a part of led project.
  Copyright (c) 2015 Rudolph Miller (chopsticks.tk.ppfm@gmail.com)
|#

(in-package :cl-user)
(defpackage led-asd
  (:use :cl :asdf))
(in-package :led-asd)

(defsystem led
  :version "0.1"
  :author "Rudolph Miller"
  :license "MIT"
  :homepage "https://github.com/Rudolph-Miller/led"
  :depends-on (:alexandria
               :cl-charms
               :babel
               :bordeaux-threads)
  :components ((:module "src"
                :serial t
                :components
                ((:module "internal"
                  :serial t
                  :components
                  ((:file "attribute")
                   (:file "character")
                   (:file "line")
                   (:file "string")
                   (:file "key")
                   (:file "package")))
                 (:module "window"
                  :serial t
                  :components
                  ((:file "window")
                   (:file "input")
                   (:file "package")))
                 (:module "buffer"
                  :serial t
                  :components
                  ((:file "buffer")
                   (:file "file-buffer")
                   (:file "package")))
                 (:module "mapping"
                  :serial t
                  :components
                  ((:file "util")
                   (:file "default")
                   (:file "package")))
                 (:file "led"))))
  :description "[WIP] LED."
  :long-description
  #.(with-open-file (stream (merge-pathnames
                             #p"README.md"
                             (or *load-pathname* *compile-file-pathname*))
                            :if-does-not-exist nil
                            :direction :input)
      (when stream
        (let ((seq (make-array (file-length stream)
                               :element-type 'character
                               :fill-pointer t)))
          (setf (fill-pointer seq) (read-sequence seq stream))
          seq)))
  :in-order-to ((test-op (test-op led-test))))
