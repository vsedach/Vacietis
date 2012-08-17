(in-package #:vacietis.test.program)

(eos:in-suite vacietis.test::program-tests)

(program-test main-return :return-code 7)

(program-test main-return-include :return-code 8)

(program-test main-return-include1 :return-code 9)

(program-test include-libc :return-code 64)

(program-test define-function1 :return-code 2)

(program-test if-then-else1 :return-code 21)
