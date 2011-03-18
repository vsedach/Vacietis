(in-package #:vacietis.test)

(in-suite vacietis-reader)

(reader-test decimal
  "1234567890"
  1234567890)

(reader-test float
  "12323.0"
  12323.0)

(reader-test string1
  "\"foo\""
  "foo")

(reader-test string2
  "\"foo\" \"bar\""
  "foobar")

;; (reader-test unclosed-string
;;   "\"foo")

(reader-test string-escape1
  "\"foo\\nbar\""
  "foo
bar")
