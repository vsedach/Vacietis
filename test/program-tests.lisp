(in-package #:vacietis.test.program)

(eos:in-suite vacietis.test::program-tests)

(program-test main-return :return-code 7)

(program-test main-return-include :return-code 8)

(program-test main-return-include1 :return-code 9)

(program-test include-libc :return-code 64)

(program-test define-function1 :return-code 2)

(program-test if-then-else1 :return-code 21)

(program-test if-then-else2 :return-code 5)

(program-test different-comment-styles :return-code 9)

(program-test hello-world0
              :output "hello world")

(program-test inc-deref-associativity
              :output "123")

;; (program-test kr-echo :input "foobar" :output "foobar")
;; (program-test hello-world :output "hello world")
;; (program-test hardward-ex3)
