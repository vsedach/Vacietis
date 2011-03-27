(in-package #:cl)

(defpackage #:vacietis.test
  (:use #:cl #:named-readtables #:eos #:vacietis #:vacietis.c)
  (:shadowing-import-from #:vacietis.c #:!)
  (:export #:run-tests))
