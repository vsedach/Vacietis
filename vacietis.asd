;;;; -*- lisp -*-

(defsystem :vacietis
  :name "vacietis"
  :author "Vladimir Sedach <vsedach@gmail.com>"
  :description "C to Common Lisp compiler"
  :licence "LLGPL v3 or later"
  :components
  ((:static-file "vacietis.asd")
   (:static-file "vacietis.test.asd")
   (:module :compiler
            :serial t
            :components
            ((:file "package")
             (:file "state")
             (:file "implementation")
             (:file "libc-support")
             (:file "type")
             (:file "reader")))
   (:module :libc
            :serial t
            :components
            ((:file "package")
             (:file "errno")
             (:file "stddef")
             (:file "stdarg")
             (:static-file "stdarg.h")
             (:file "ctype")
             (:file "math")
             (:file "stdio")
             (:static-file "scanf.c")
             (:file "stdlib")
             (:file "string")
             (:static-file "string.c"))
            :depends-on (:compiler))
   (:module :runtime
            :components
            ((:file "program"))
            :depends-on (:compiler :libc)))
  :depends-on (:named-readtables :anaphora :babel :cl-ppcre :cl-fad))
