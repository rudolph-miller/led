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
           :make-bd
           :make-top-bd
           :top-bd-p
           :bd-length
           :bd-index+
           :bd-index-
           :bd-index=
           :bd-index<
           :bd-index<=
           :bd-index>
           :bd-index>=
           :prev
           :next
           :bd-value
           :bd-index
           :iterate-bd
           :iterate-to-end
           :get-by-index
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
