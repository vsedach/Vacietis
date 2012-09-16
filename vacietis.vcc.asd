;;;; -*- lisp -*-

(defsystem :vacietis.vcc
  :author "Vladimir Sedach <vsedach@gmail.com"
  :license "LLGPL v3 or later"
  :components
  ((:module :vcc
            :serial t
            :components ((:file "package")
                         (:file "vcc"))))
  :depends-on (:vacietis))
