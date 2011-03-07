(in-package #:vacietis.test)

(in-suite vacietis-reader)

(reader-test decimal
  "1234567890"
  1234567890)

(reader-test float
  "12323.0"
  12323.0)
