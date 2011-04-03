(in-package #:vacietis.test.basic)
(in-readtable vacietis:vacietis)

(in-suite vacietis.test::basic-tests)

(eval-test addition0
  "1 + 2;"
  3)

(eval-test subtraction0
  "3-2;"
  1)
