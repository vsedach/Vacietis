;;;; -*- lisp -*-

(defsystem :vacietis
  :name "vacietis"
  :author "Vladimir Sedach <vsedach@gmail.com>"
  :description "C to Common Lisp compiler"
  :licence "LLGPLv3"
  :components
  ((:static-file "vacietis.asd")
   (:static-file "vacietis.test.asd")
   (:module :compiler
            :serial t
            :components
            ((:file "package")
             (:file "types")
             (:file "implementation")
             (:file "include")))
   (:module :reader
            :serial t
            :components
            ((:file "package")
             (:file "reader"))
            :depends-on (:compiler))
   (:module :libc
            :serial t
            :components
            ((:file "package")
             (:file "errno")
             (:file "stddef")
             (:file "ctype")
             (:file "math")
             (:file "stdio")
             (:file "stdlib")
             (:file "string")
             (:static-file "string.c"))
            :depends-on (:compiler :reader))
   (:module :runtime
            :components
            ((:file "program"))
            :depends-on (:compiler :libc)))
  :depends-on (:named-readtables :anaphora :babel :cl-ppcre :cl-fad))
