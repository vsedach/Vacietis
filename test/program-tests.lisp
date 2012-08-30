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

(program-test hello-world0 :output "hello world")

(program-test hello-world :output "hello world
")

(program-test inc-deref-associativity :output "123")

(program-test hardway-ex3 :output
              "I am 10 years old.
I am 72 inches tall.
")

(program-test kr-echo :input "foobar" :output "foobar")

(program-test hanly-83 ;; modify when scanf is added
              :output
              "Enter 8 numbers separated by blanks or <return>s
> The mean is 2.00.
The standard deviation is 21.75.

Table of differences between data values and mean
Index    Item    Difference
  0        16.00         14.00
  1        12.00         10.00
  2         6.00          4.00
  3         8.00          6.00
  4         2.50          0.50
  5        12.00         10.00
  6        14.00         12.00
  7       -54.50        -56.50
")

;; needs scanf
;; (program-test hanly-113
;;               :input "200"
;;               :output "321.8")




