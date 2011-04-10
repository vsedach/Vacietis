(cl:in-package #:vacietis.test.reader)
(named-readtables:in-readtable vacietis:vacietis)

(eos:in-suite vacietis.test::vacietis-reader)

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

(reader-test identifier1
  "_foo;"
  _foo)

(reader-test identifier2
  "bar_foo;"
  bar_foo)

(reader-test identifier3
  "bar_foo99;"
  bar_foo99)

(reader-test int-var1
  "int x;"
  (cl:progn (cl:defvar x)))

;;; function definition

(reader-test simple-function1
  "void foo(int a, int b) {
a + b;
}"
  (cl:defun foo (a b)
    (cl:let ()
     (cl:tagbody (+ a b)))))

(reader-test function0
  "int max(int a, int b)
{
return a > b ? a : b;
}"
  (cl:defun max (a b)
    (cl:let ()
     (cl:tagbody (return (if (> a b) a b))))))

(reader-test function1
  "extern int max(int a, int b)
{
return a > b ? a : b;
}"
  (cl:defun max (a b)
    (cl:let ()
     (cl:tagbody (return (if (> a b) a b))))))

;; (reader-test function2 ;; yes this is legal
;;   "extern int max(a, b)
;; int a, b;
;; {
;; return a > b ? a : b;
;; }
;; ")

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

;;; expressions

(reader-test number-plus
  "1 + 2;"
  (+ 1 2))

(reader-test foo-plus
  "foo + 2;"
  (+ foo 2))

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

;;; conditionals

(reader-test if-foo1
  "if foo { 1 + 2; }"
  (if foo (cl:tagbody (+ 1 2))))

(reader-test if-foo2
  "if foo 1 + 2;"
  (if foo (+ 1 2)))

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
      (cl:tagbody
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
      (cl:tagbody 1)))

;;; casts and pointers

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

(reader-test plus-eql
  "access_control_stack_pointer(th) += 1;"
  (+= (access_control_stack_pointer th) 1))

(reader-test pointer-pointer
  "result = (int *) *access_control_stack_pointer(th);"
  (= result (deref* (access_control_stack_pointer th))))

(reader-test cast-deref
  "(int) *foo();"
  (deref* (foo)))

(reader-test declare-pointer0
  "int *result;"
  (cl:progn (cl:defvar result)))

(reader-test ptr-ptr-cast
  "(int *)((char *)result + bytes);"
  (+ result bytes))

(reader-test ptr-ptr-cast-assign
  "dynamic_space_free_pointer = (int *)((char *)result + bytes);"
  (= dynamic_space_free_pointer (+ result bytes)))

(reader-test cast-ptr-subtract
  "(char *)dynamic_space_free_pointer
                            - (char *)current_dynamic_space;"
  (- dynamic_space_free_pointer current_dynamic_space))

(reader-test funcall-arglist-op1
  "foo(1 - 2);"
  (foo (- 1 2)))

(reader-test funcall-arglist-op2
  "foo(1 - 2, 3 - 4);"
  (foo (- 1 2) (- 3 4)))

(reader-test funcall-arglist-op3
  "foo(1 - 2, 4);"
  (foo (- 1 2) 4))

(reader-test funcall-cast-ptr-subtract
  "set_auto_gc_trigger((char *)dynamic_space_free_pointer
                            - (char *)current_dynamic_space);"
  (set_auto_gc_trigger (- dynamic_space_free_pointer current_dynamic_space)))

(reader-test big-if1
  "if (current_auto_gc_trigger
        && dynamic_space_free_pointer > current_auto_gc_trigger) {
        clear_auto_gc_trigger();
        set_auto_gc_trigger((char *)dynamic_space_free_pointer
                            - (char *)current_dynamic_space);
    }"
  (if (&& current_auto_gc_trigger (> dynamic_space_free_pointer current_auto_gc_trigger))
      (cl:tagbody
        (clear_auto_gc_trigger)
        (set_auto_gc_trigger (- dynamic_space_free_pointer current_dynamic_space)))))

(reader-test deref-increment
  "*x++;"
  (deref* (post++ x)))

(reader-test no-arg-function
  "void foo() {
a + b;
}"
  (cl:defun foo ()
    (cl:let ()
     (cl:tagbody (+ a b)))))

(reader-test labeled-statement1
  "void foo() {
baz: a + b;
}"
  (cl:defun foo ()
    (cl:let ()
     (cl:tagbody baz (+ a b)))))

(reader-test sizeof-something
  "result = pa_alloc(ALIGNED_SIZE((1 + words) * sizeof(lispobj)),
                      UNBOXED_PAGE_FLAG);"
  (= result
     (pa_alloc
      (ALIGNED_SIZE
       (* (+ 1 words) (sizeof lispobj)))
      UNBOXED_PAGE_FLAG)))

(reader-test deref-cast-shift
  "*result = (int) (words << N_WIDETAG_BITS) | type;"
  (= (deref* result)
     (|\|| (<< words N_WIDETAG_BITS) type)))

(reader-test function-vars0
  "void main () {
int x;
}"
  (cl:defun main ()
    (cl:let ((x 0))
      (cl:tagbody (cl:progn (cl:setf x 0))))))

(reader-test while0
  "while (fahr <= upper) {
celsius = 5 * (fahr-32) / 9;
printf(\"%d\t%d\n\", fahr, celsius);
fahr = fahr + step;
}"
  (while (<= fahr upper)
    (cl:tagbody
       (= celsius (* 5 (/ (- fahr 32) 9)))
       (printf "%dt%dn" fahr celsius)
       (= fahr (+ fahr step)))))

(reader-test multiple-declaration0
  "int x, y;"
  (cl:progn (cl:defvar y) (cl:defvar x)))

(reader-test k&r-pg12
  "void main()
{
int fahr, celsius;
int lower, upper, step;
lower = 0;
upper = 300;
step = 20;
/* lower limit of temperature scale */
/* upper limit */
/* step size */
fahr = lower;
while (fahr <= upper) {
celsius = 5 * (fahr-32) / 9;
printf(\"%d\t%d\n\", fahr, celsius);
fahr = fahr + step;
}
}
"
  (cl:defun main ()
    (cl:let ((step 0) (upper 0) (lower 0) (celsius 0) (fahr 0))
      (cl:tagbody
         (cl:progn (setf celsius 0) (setf fahr 0))
         (cl:progn (setf step 0) (setf upper 0) (setf lower 0))
         (= lower 0)
         (= upper 300)
         (= step 20)
         (= fahr lower)
         (while (<= fahr upper)
           (cl:tagbody
              (= celsius (* 5 (/ (- fahr 32) 9)))
              (printf "%dt%dn" fahr celsius)
              (= fahr (+ fahr step))))))))
