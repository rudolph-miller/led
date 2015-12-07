(in-package :cl-user)
(defpackage led.internal
  (:use :led.internal.bidirectional-link
        :led.internal.character
        :led.internal.line
        :led.internal.string
        :led.internal.mode
        :led.internal.key)
  (:export ;; bidirectional-link
           :bdl
           :make-bdl
           :make-top-bdl
           :top-bdl-p
           :bdl-length
           :bdl-index+
           :bdl-index-
           :bdl-index=
           :bdl-index<
           :bdl-index<=
           :bdl-index>
           :bdl-index>=
           :prev
           :next
           :bdl-value
           :bdl-index
           :iterate-bdl
           :iterate-to-end
           :get-by-index
           :insert-prev
           :insert-next
           :delete-bdl
   
           ;; character
           :ichar
           :ichar-char
           :ichar-attr
           :character-to-ichar
           :char-width
           :ichar-width
           :ichars-width
           :ichars-with-padding

           ;; bdl-ichar
           ;; line
           :make-line
           :line-index
           :line-ichars
           :ichar-position
           :make-top-line
           :line-length
           :line-ichars-length
           :delete-ichar-of-line
           :replace-ichar-of-line
           :insert-ichar-to-line
           :string-to-line
           :iterate-lines

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
