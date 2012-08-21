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
             (:file "compiler-data")
             (:file "implementation")
             (:file "include")
             (:file "reader")))
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
            :depends-on (:compiler))
   (:module :runtime
            :components
            ((:file "program"))
            :depends-on (:compiler :libc)))
  :depends-on (:named-readtables :anaphora :babel :cl-ppcre :cl-fad))
