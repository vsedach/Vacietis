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
   (:module :src
            :components
            ((:file "package")
             (:file "types")
             (:file "compiler-data")
             (:file "runtime-data")
             (:file "implementation")
             (:file "reader"))))
  :depends-on (:named-readtables :anaphora :babel :cl-ppcre :cl-fad))
