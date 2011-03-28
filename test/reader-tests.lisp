(in-package #:vacietis.test)
(in-readtable vacietis)

(in-suite vacietis-reader)

(reader-test decimal
  "1234567890;"
  1234567890)

(reader-test float
  "12323.0;"
  12323.0)

(reader-test zero
  "0;"
  0)

(reader-test zero-float
  "0.0;"
  0.0)

(reader-test string1
  "x = \"foo\";"
  (= x "foo"))

(reader-test string2
  "b = \"foo\" \"bar\";"
  (= b "foobar"))

;; (reader-test unclosed-string
;;   "\"foo")

(reader-test string-escape1
  "_FOO = \"foo\\nbar\";"
  (= _FOO "foo
bar"))

;; (reader-test arglist1
;;   "(int x, int y)"
;;   (int x int y))

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

(reader-test elvis0
  "a ? 1 : 2;"
  (if a 1 2))

(reader-test elvis1
  "a > b ? a : b;"
  (if (> a b) a b))

(reader-test elvis-return
  "return a > b ? a : b;"
  (return (if (> a b) a b)))

(reader-test return1
  "return 1;"
  (return 1))

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

(reader-test lognot1
  "foo = ~010;"
  (= foo (~ 8)))

(reader-test nequal1
  "foo != 0x10;"
  (!= foo 16))

(reader-test inc1
  "++a;"
  (++ a))

(reader-test inc2
  "a++;"
  (post++ a))

(reader-test dec1
  "--a;"
  (-- a))

(reader-test dec2
  "a--;"
  (post-- a))

(reader-test dec3
  "--foo;"
  (-- foo))

(reader-test op-precedence1
  "a + b + c;"
  (+ a (+ b c)))

(reader-test assign1
  "foo = 1;"
  (= foo 1))

(reader-test assign2
  "foo = 1 + 2;"
  (= foo (+ 1 2)))

(reader-test assign3
  "foo = !2;"
  (= foo (! 2)))

(reader-test assign4
  "foo = ~2;"
  (= foo (~ 2)))

;;; function calls

(reader-test funcall-args0
  "random();"
  (random))

(reader-test funcall-args1
  "foo(1);"
  (foo 1))

(reader-test funcall-args2
  "foo(1,2);"
  (foo 1 2))

(reader-test funcall-args3
  "foo(1,2,3);"
  (foo 1 2 3))

(reader-test funcall-args4
  "foo(1,2,3,4);"
  (foo 1 2 3 4))

(reader-test function-call1
  "printf(\"hello, world\\n\");"
  (printf "hello, world
"))

(reader-test function-call2
  "check_gc_signals_unblocked_or_lose(0);"
  (check_gc_signals_unblocked_or_lose 0))

(reader-test function-call-assign0
  "result = general_alloc(bytes, page_type_flag);"
  (= result (general_alloc bytes page_type_flag)))

(reader-test multi-line-exp0
  "(SymbolValue(GC_PENDING,th) == NIL) &&
   (SymbolValue(GC_INHIBIT,th) == NIL) &&
   (random() < RAND_MAX/100);"
  (&& (== (SymbolValue GC_PENDING th) NIL)
      (&& (== (SymbolValue GC_INHIBIT th) NIL)
          (< (random) (/ RAND_MAX 100)))))

(reader-test funcall-compare
  "SymbolValue(GC_PENDING,th) == NIL;"
  (== (SymbolValue GC_PENDING th) NIL))

(reader-test funcall-compare-parethesized
  "(SymbolValue(GC_PENDING,th) == NIL);"
  (== (SymbolValue GC_PENDING th) NIL))

(reader-test funcall-lessthan
  "random() < RAND_MAX/100;"
  (< (random) (/ RAND_MAX 100)))

(reader-test multi-exp0
  "(SymbolValue(GC_PENDING,th) == NIL) &&
   (SymbolValue(GC_INHIBIT,th) == NIL);"
  (&& (== (SymbolValue GC_PENDING th) NIL) (== (SymbolValue GC_INHIBIT th) NIL)))

(reader-test big-if
  "if ((SymbolValue(GC_PENDING,th) == NIL) &&
        (SymbolValue(GC_INHIBIT,th) == NIL) &&
        (random() < RAND_MAX/100)) {
        SetSymbolValue(GC_PENDING,T,th);
        set_pseudo_atomic_interrupted(th);
        maybe_save_gc_mask_and_block_deferrables(NULL);
    }"
  (if (&& (== (SymbolValue GC_PENDING th) NIL)
          (&& (== (SymbolValue GC_INHIBIT th) NIL)
              (< (random) (/ RAND_MAX 100))))
      (progn
        (SetSymbolValue GC_PENDING T th)
        (set_pseudo_atomic_interrupted th)
        (maybe_save_gc_mask_and_block_deferrables NULL))))

(reader-test smaller-if
  "if ((SymbolValue(GC_PENDING,th) == NIL) &&
        (SymbolValue(GC_INHIBIT,th) == NIL) &&
        (random() < RAND_MAX/100)) {
1;
    }"
  (if (&& (== (SymbolValue GC_PENDING th) NIL)
          (&& (== (SymbolValue GC_INHIBIT th) NIL)
              (< (random) (/ RAND_MAX 100))))
      (progn 1)))

(reader-test cast1
  "(int) foobar;"
  foobar)

(reader-test deref-var
  "*foo;"
  (deref* foo))

(reader-test deref-funcall
  "*foo();"
  (deref* (foo)))

(reader-test deref-assign-cast
  "*access_control_stack_pointer(th) = (int) result;"
  (= (deref* (access_control_stack_pointer th)) result))
