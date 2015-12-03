(in-package :cl-user)
(defpackage led.internal
  (:use :led.internal.character
        :led.internal.line
        :led.internal.string
        :led.internal.mode
        :led.internal.key)
  (:export ;; character
           :ichar
           :ichar-val
           :ichar-attr
           :character-to-ichar

           ;; line
           :make-line
           :line-ichars
           :line-eol-p
           :line-length
           :line-ichars-with-padding
           :string-to-line

           ;; string
           :string-to-lines
           :lines-to-string

           ;; mode
           :*modes*
           :*current-mode*
           :*mode-changed-hooks*
           :current-mode

           ;; key
           :*global-key-mapping*
           :get-namespace
           :create-namespace
           :register-key
           :unregister-key))
