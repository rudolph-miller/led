(in-package :cl-user)
(defpackage led.internal
  (:use :led.internal.bidirectional-link
        :led.internal.ichar
        :led.internal.bdl-ichar
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
           :get-by-index
           :insert-prev
           :insert-next
           :replace-bdl
           :delete-bdl
   
           ;; ichar
           :ichar
           :ichar-char
           :ichar-attr
           :character-to-ichar
           :char-width
           :ichar-width
           :ichars-width
           :ichars-with-padding

           ;; bdl-ichar
           :bdl-ichar
           :bdl-ichar-ichar
           :make-top-bdl-ichar
           :make-dbl-ichar
           :insert-prev-bdl-ichar
           :insert-next-bdl-ichar
           :replace-bdl-ichar
           :delete-bdl-ichar
           :bdl-ichar-length
           :iterate-bdl-ichars

           ;; line
           :make-line
           :line-index
           :line-top-bdl-ichar
           :bdl-ichar-line
           :make-top-line
           :line-length
           :line-bdl-ichars-length
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
