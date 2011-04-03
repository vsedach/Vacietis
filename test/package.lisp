(in-package #:cl)

(defpackage #:vacietis.test
  (:use #:cl #:named-readtables #:eos #:vacietis)
  (:export #:run-tests
           #:eval-test
           #:reader-test))

(defpackage #:vacietis.test.reader
  (:use #:vacietis.c #:vacietis.test))

(defpackage #:vacietis.test.basic
  (:use #:cl #:named-readtables #:eos #:vacietis.test))
