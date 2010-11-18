;;;; -*- lisp -*-

(defsystem :vacietis
  :name "vacietis"
  :author "Vladimir Sedach <vsedach@gmail.com>"
  :licence "LLGPL"
  :serial t
  :components
  ((:static-file "vacietis.asd")
   (:static-file "vacietis.test.asd")
   (:file "package")
   (:file "pointers")
   (:file "reader")))
