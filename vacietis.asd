;;;; -*- lisp -*-

(defsystem :vacietis
  :name "vacietis"
  :author "Vladimir Sedach <vsedach@gmail.com>"
  :description "C to Common Lisp compiler"
  :licence "LLGPLv3"
  :serial t
  :components
  ((:static-file "vacietis.asd")
   (:static-file "vacietis.test.asd")
   (:module :compiler
            :components
            ((:file "package")
             (:file "types")
             (:file "compiler-data")
             (:file "implementation")
             (:file "reader")))
   (:module :libc
            :components
            ((:file "package")
             (:file "errno")
             (:file "stddef")
             (:file "ctype")
             (:file "math")
             (:file "stdio")
             (:file "stdlib")
             (:file "string"))
            :depends-on (:compiler))
   (:module :runtime
            :components
            ((:file "program"))
            :depends-on (:compiler :libc)))
  :depends-on (:named-readtables :anaphora :babel :cl-ppcre :cl-fad))
