(in-package #:cl)

(defpackage #:vacietis.test
  (:use #:cl #:named-readtables #:eos #:vacietis)
  (:export #:run-tests))

(defpackage #:vacietis.test.reader
  (:use #:vacietis.c))
