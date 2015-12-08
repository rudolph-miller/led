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
           :get-first
           :get-last
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
           :bdl-ichar-index
           :make-top-bdl-ichar
           :make-dbl-ichar
           :bdl-ichar-length
           :iterate-bdl-ichars
           :bdl-ichars-to-ichars

           ;; line
           :make-line
           :line-index
           :line-top-bdl-ichar
           :bdl-ichar-line
           :make-top-line
           :line-length
           :line-bdl-ichars-length
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
