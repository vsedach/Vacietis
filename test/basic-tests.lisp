(in-package #:vacietis.test.basic)
(in-readtable vacietis:vacietis)

(in-suite vacietis.test::basic-tests)

(eval-test addition0
  "1 + 2;"
  3)

(eval-test subtraction0
  "3-2;"
  1)

(eval-test global-var
  "int foobar = 10;
foobar;"
  10)

;; (eval-test for-loop0
;;   "int foobar;
;; for (int x = 0, foobar = 0; x <= 10; x++) foobar += x;
;; foobar;"
;;   55) ;; comes out to 0 because of foobar scope, bug or feature?

(eval-test for-loop1
  "int foobar = 0;
for (int x = 0; x <= 10; x++) foobar += x;
foobar;"
  55)

(eval-test string-literal
  "char foobar[] = \"foobar\";
foobar;"
  (string-to-char* "foobar"))

(eval-test h&s-while-string-copy
  "char source_pointer[] = \"foobar\", dest_pointer[7];
while ( *dest_pointer++ = *source_pointer++ );
dest_pointer - 7;"
  (string-to-char* "foobar"))
