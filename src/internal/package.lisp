(in-package :cl-user)
(defpackage led.internal
  (:use :led.internal.bidirectional-link
        :led.internal.character
        :led.internal.line
        :led.internal.string
        :led.internal.mode
        :led.internal.key)
  (:export ;; bidirectional-link
           :bd
           :make-new-bd
           :iterate-bd
           :iterate-to-end
           :insert-prev
           :insert-next
           :delete-bd
   
           ;; character
           :ichar
           :ichar-char
           :ichar-attr
           :character-to-ichar
           :char-width
           :ichar-width
           :ichars-width
           :ichars-with-padding

           ;; line
           :make-line
           :line-ichars
           :line-eol-p
           :line-length
           :delete-ichar-of-line
           :replace-ichar-of-line
           :insert-ichar-to-line
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
           :char-dsl
           :dsl-char
           :get-namespace
           :create-namespace
           :register-key
           :unregister-key))
