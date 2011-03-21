(in-package #:vacietis.test)

(in-suite vacietis-reader)

(reader-test decimal
  "1234567890;"
  1234567890)

(reader-test float
  "12323.0;"
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

;; (reader-test arglist1
;;   "(int x, int y)"
;;   (int x int y))

(reader-test decrement1
  "--foo;"
  (vacietis.c:-- foo))

(reader-test identifier1
  "_foo;"
  _foo)

(reader-test identifier2
  "bar_foo;"
  bar_foo)

(reader-test identifier3
  "bar_foo99;"
  bar_foo99)

(reader-test number-plus
  "1 + 2;"
  (+ 1 2))

(reader-test foo-plus
  "foo + 2;"
  (+ foo 2))

(reader-test if-foo1
  "if foo { 1 + 2; }"
  (if foo (progn (+ 1 2))))

(reader-test if-foo2
  "if foo 1 + 2;"
  (if foo (+ 1 2)))

(reader-test int-var1
  "int x;"
  (defvar x))

(reader-test simple-function1
  "void foo(int a, int b) {
a + b;
}"
  (defun foo (a b)
    (progn (+ a b))))

;; (reader-test function1
;;   "extern int max(int a, int b)
;; {
;; return a > b ? a : b;
;; }")

;; (reader-test function2 ;; yes this is legal
;;   "extern int max(a, b)
;; int a, b;
;; {
;; return a > b ? a : b;
;; }
;; ")