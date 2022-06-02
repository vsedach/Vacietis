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
  (= x (string-to-char* "foo")))

(reader-test string2
  "b = \"foo\" \"bar\";"
  (= b (string-to-char* "foobar")))

(reader-test string-escape1
  "_FOO = \"foo\\nbar\";"
  ;; yup, \n is an escape and not a format string!
  (= _FOO (string-to-char* "foo
bar")))

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
  (cl:progn (cl:defparameter x 0)))

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
  (printf (string-to-char* "hello, world
")))

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
  (cl:if (cl:not (cl:eql 0 a)) 1 2))

(reader-test elvis1
  "a > b ? a : b;"
  (cl:if (cl:not (cl:eql 0 (> a b))) a b))

(reader-test elvis-return
  "return a > b ? a : b;"
  (cl:return (cl:if (cl:not (cl:eql 0 (> a b))) a b)))

(reader-test return1
  "return 1;"
  (cl:return 1))

(reader-test lognot1
  "foo = ~010;"
  (= foo (~ 8)))

(reader-test nequal1
  "foo != 0x10;"
  (!= foo 16))

(reader-test inc1
  "++a;"
  (= a (+ a 1)))

(reader-test inc2
  "a++;"
  (cl:prog1 a (= a (+ a 1))))

(reader-test dec1
  "--a;"
  (= a (- a 1)))

(reader-test dec2
  "a--;"
  (cl:prog1 a (= a (- a 1))))

(reader-test dec3
  "--foo;"
  (= foo (- foo 1)))

(reader-test op-precedence1
  "a + b + c;"
  (+ (+ a b) c))

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
  (&& (&& (== (SymbolValue GC_PENDING th) NIL)
          (== (SymbolValue GC_INHIBIT th) NIL))
      (< (random) (/ RAND_MAX 100))))

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
  (cl:if (cl:eql 0 foo)
         cl:nil
         (cl:tagbody (+ 1 2))))

(reader-test if-foo2
  "if foo 1 + 2;"
  (cl:if (cl:eql 0 foo)
         cl:nil
         (+ 1 2)))

(reader-test big-if
  "if ((SymbolValue(GC_PENDING,th) == NIL) &&
        (SymbolValue(GC_INHIBIT,th) == NIL) &&
        (random() < RAND_MAX/100)) {
        SetSymbolValue(GC_PENDING,T,th);
        set_pseudo_atomic_interrupted(th);
        maybe_save_gc_mask_and_block_deferrables(NULL);
    }"
  (cl:if (cl:eql 0
                 (&& (&& (== (SymbolValue GC_PENDING th) NIL)
                         (== (SymbolValue GC_INHIBIT th) NIL))
                     (< (random) (/ RAND_MAX 100))))
         cl:nil
         (cl:tagbody
            (SetSymbolValue GC_PENDING T th)
            (set_pseudo_atomic_interrupted th)
            (maybe_save_gc_mask_and_block_deferrables vacietis.test.reader::NULL))))

(reader-test smaller-if
  "if ((SymbolValue(GC_PENDING,th) == NIL) &&
        (SymbolValue(GC_INHIBIT,th) == NIL) &&
        (random() < RAND_MAX/100)) {
1;
    }"
  (cl:if (cl:eql 0
                 (&& (&& (== (SymbolValue GC_PENDING th) NIL)
                         (== (SymbolValue GC_INHIBIT th) NIL))
                     (< (random) (/ RAND_MAX 100))))
         cl:nil
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
  (cl:progn (cl:defparameter result 0)))

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
  (cl:if (cl:eql 0 (&& current_auto_gc_trigger
                       (> dynamic_space_free_pointer current_auto_gc_trigger)))
         cl:nil
         (cl:tagbody
            (clear_auto_gc_trigger)
            (set_auto_gc_trigger
             (- dynamic_space_free_pointer current_dynamic_space)))))

(reader-test deref-increment
  "*x++;"
  (deref* (cl:prog1 x (= x (+ x 1)))))

(reader-test sizeof-something
  "int lispobj[20];
result = pa_alloc(ALIGNED_SIZE((1 + words) * sizeof(lispobj)),
                      UNBOXED_PAGE_FLAG);"
  (cl:progn (cl:defparameter lispobj (vacietis:allocate-memory 20)))
  (= result
     (pa_alloc
      (ALIGNED_SIZE
       (* (+ 1 words) 20))
      UNBOXED_PAGE_FLAG)))

(reader-test deref-cast-shift
  "*result = (int) (words << N_WIDETAG_BITS) | type;"
  (= (deref* result)
     (|\|| (<< words N_WIDETAG_BITS) type)))

(reader-test function-vars0
  "void main () {
int x;
}"
  (vacietis::defun/1 main ()
    (cl:prog* ((x 0))))
  )

(reader-test function-comments0
  "void main () {
/* this is a comment */
int x;
}"
  (vacietis::defun/1 main ()
    (cl:prog* ((x 0))))
  )

(reader-test function-comments1
  "void main () {
/* this is a comment */
int x;
// this is another comment
}"
  (vacietis::defun/1 main ()
    (cl:prog* ((x 0))))
  )

(reader-test while0
  "while (fahr <= upper) {
celsius = 5 * (fahr-32) / 9;
printf(\"%d\\t%d\\n\", fahr, celsius);
fahr = fahr + step;
}"
  (for (cl:nil cl:nil (<= fahr upper) cl:nil)
    (cl:tagbody
       (= celsius (/ (* 5 (- fahr 32)) 9))
       (printf (string-to-char* "%d	%d
") fahr celsius)
       (= fahr (+ fahr step)))))

(reader-test multiple-declaration0
  "int x, y;"
  (cl:progn (cl:defparameter x 0) (cl:defparameter y 0)))

(reader-test k&r-pg9
  "void main()
{
printf(\"hello, world\\n\");
}
"
  (vacietis::defun/1 main ()
    (cl:prog* ()
       (printf (string-to-char* "hello, world
")))))

(reader-test c99-style-for-init
  "for (int x = 0; x < 10; x++)
x++;"
  (for (((x 0)) (cl:progn (= x 0)) (< x 10) (cl:prog1 x (= x (+ x 1))))
    (cl:prog1 x (= x (+ x 1)))))

(reader-test c99-style-for1
  "for (int x = 0; x < 10; x++) foobar += x;"
  (for (((x 0))
        (cl:progn (= x 0))
        (< x 10)
        (cl:prog1 x (= x (+ x 1))))
    (+= foobar x)))

(reader-test var-declare-and-initialize0
  "int x = 1;"
  (cl:progn (cl:defparameter x 1)))

(reader-test modulo0
  "1 % 2;"
  (% 1 2))

(reader-test empty-label
  "int main () { end:; }"
  (vacietis::defun/1 main ()
    (cl:prog* ()
     end
     cl:nil)))

(reader-test h&s-while2
  "while ( *char_pointer++ );"
  (for (cl:nil
        cl:nil
        (deref* (cl:prog1 char_pointer
                  (= char_pointer (+ char_pointer 1))))
        cl:nil)
    cl:nil))

(reader-test h&s-while3
  "while ( *dest_pointer++ = *source_pointer++ );"
  (for (cl:nil
        cl:nil
        (= (deref* (cl:prog1 dest_pointer
                     (= dest_pointer (+ dest_pointer 1))))
           (deref* (cl:prog1 source_pointer
                     (= source_pointer (+ source_pointer 1)))))
        cl:nil)
    cl:nil))

(reader-test just-return
  "return;"
  (cl:return 0))


(reader-test pointer-to-array-of-ints
  "int (*foobar)[];" ;; ok not to specify size of array here
  (cl:progn (cl:defparameter foobar 0)))

(reader-test array-of-pointers-to-int1
  "int *foobar[5];"
  (cl:progn (cl:defparameter foobar (vacietis:allocate-memory 5))))

(reader-test array-of-ints1
  "int foobar[5];"
  (cl:progn (cl:defparameter foobar (vacietis:allocate-memory 5))))

(reader-test pointer-to-int0
  "int *x;"
  (cl:progn (cl:defparameter x 0)))

(reader-test char-literal0
  "char foobar[] = \"Foobar\";"
  (cl:progn (cl:defparameter foobar (string-to-char* "Foobar"))))

(reader-test declaration-initialization0
  "int x = 1 + 2;"
  (cl:progn (cl:defparameter x (+ 1 2))))

(reader-test declare-two-ints0
  "int x, y;"
  (cl:progn (cl:defparameter x 0)
            (cl:defparameter y 0)))

(reader-test declare-two-ints-initialize0
  "int x = 1, y;"
  (cl:progn (cl:defparameter x 1)
            (cl:defparameter y 0)))

(reader-test declare-two-ints-initialize1
  "int x, y = 1;"
  (cl:progn (cl:defparameter x 0)
            (cl:defparameter y 1)))

(reader-test declare-two-ints-initialize2
  "int x = 1, y = 2;"
  (cl:progn (cl:defparameter x 1)
            (cl:defparameter y 2)))

(reader-test declare-two-ints-initialize3
  "int x = 1 + 2, y;"
  (cl:progn (cl:defparameter x (+ 1 2))
            (cl:defparameter y 0)))

(reader-test declare-two-ints-initialize4
  "int x, y = 1 + 2;"
  (cl:progn (cl:defparameter x 0)
            (cl:defparameter y (+ 1 2))))

(reader-test declare-two-ints-initialize5
  "int x = 1 + 2, y = 3 + 4;"
  (cl:progn (cl:defparameter x (+ 1 2))
            (cl:defparameter y (+ 3 4))))

(reader-test declare-two-ints-initialize6
  "int x = foo(), y;"
  (cl:progn (cl:defparameter x (foo))
            (cl:defparameter y 0)))

(reader-test declare-two-ints-initialize7
  "int x = foo(1 + 2), y;"
  (cl:progn (cl:defparameter x (foo (+ 1 2)))
            (cl:defparameter y 0)))

(reader-test declare-two-ints-initialize8
  "int x, y = foo(1 + 2);"
  (cl:progn (cl:defparameter x 0)
            (cl:defparameter y (foo (+ 1 2)))))

(reader-test declare-two-ints-initialize9
  "int x = 3 + 4, y = foo(1 + 2);"
  (cl:progn (cl:defparameter x (+ 3 4))
            (cl:defparameter y (foo (+ 1 2)))))

(reader-test declare-two-ints-initialize10
  "int x = bar(3 + 4), y = foo(1 + 2);"
  (cl:progn (cl:defparameter x (bar (+ 3 4)))
            (cl:defparameter y (foo (+ 1 2)))))

(reader-test declare-array-of-pointers
  "int *x[10];"
  (cl:progn (cl:defparameter x (vacietis:allocate-memory 10))))

(reader-test declare-pointer-to-array
  "int (*x)[10];" ;; pointer to array of 10 integers
  (cl:progn (cl:defparameter x 0)))

(reader-test declare-deref1
  "int *x[4], *y[] = { 7, 11 };"
  (cl:progn (cl:defparameter x (vacietis:allocate-memory 4))
            (cl:defparameter y (vacietis::make-memptr :mem (cl:vector 7 11)))))

(reader-test declare-deref2
  "int *x[2], *y = 4;"
  (cl:progn (cl:defparameter x (vacietis:allocate-memory 2))
            (cl:defparameter y 4)))

(reader-test declare-deref3
  "int *x[2], *y;"
  (cl:progn (cl:defparameter x (vacietis:allocate-memory 2))
            (cl:defparameter y 0)))

(reader-test declare-deref4
  "int *x[2], y;"
  (cl:progn (cl:defparameter x (vacietis:allocate-memory 2))
            (cl:defparameter y 0)))

(reader-test declare-deref5
  "int x[1234], y;"
  (cl:progn (cl:defparameter x (vacietis:allocate-memory 1234))
            (cl:defparameter y 0)))

(reader-test declare-two-chars-initialize0
  "char source_pointer[] = \"foobar\", dest_pointer[7];"
  (cl:progn
    (cl:defparameter source_pointer (string-to-char* "foobar"))
    (cl:defparameter dest_pointer (vacietis:allocate-memory 7))))

(reader-test aref0
  "x[5];"
  ([] x 5))

(reader-test aref1
  "x[1 + 2];"
  ([] x (+ 1 2)))

(reader-test h&s-static-short
  "static short s;"
  (cl:progn (cl:defparameter s 0)))

(reader-test h&s-declaration-multiple-initialization
  "void main() {
static short s;
auto short *sp = &s + 3, *msp = &s - 3;
}"
  (vacietis::defun/1 main ()
    (cl:prog* ((msp 0) (sp 0) (s 0))
       (cl:progn
         (= sp (+ (mkptr& s) 3))
         (= msp (- (mkptr& s) 3))))))

(reader-test deref-op-precedence
  "&p + 1;"
  (+ (mkptr& p) 1))

(reader-test mkptr-increment
  "&p++;"
  (mkptr& (cl:prog1 p (= p (+ p 1)))))

(reader-test preprocessor-define-template-noargs
  "#define getchar()  getc(stdin)
getchar();"
  (getc stdin))

(reader-test function-returns-pointer
  "char *strrchr( const  char *s, int c)
{
   return 2;
}"
  (vacietis::defun/1 strrchr (s c)
    (cl:prog* cl:nil
       (cl:return 2))))

(reader-test deref-exp
  "*(foo + 1)"
  (deref* (+ foo 1)))

(reader-test deref-exp1
  "*(s += strspn(s, s2))"
  (deref* (+= s (strspn s s2))))

(reader-test negative-number
  "-1;"
  (cl:- 1))

(reader-test negative-exp
  "-(a * b);"
  (cl:- (* a b)))

(reader-test not-funcall
  "!foo()"
  (! (foo)))

(reader-test not-deref
  "!*p"
  (! (deref* p)))

(reader-test notnot
  "!!p"
  (! (! p)))

(reader-test notnotnot
  "!!!p"
  (! (! (! p))))

(reader-test notnotnotnot
  "!!!!p"
  (! (! (! (! p)))))

(reader-test two-deref
  "**p"
  (deref* (deref* p)))

(reader-test parenthezation
  "(2+3) * 6;"
  (* (+ 2 3) 6))

(reader-test compare-plusplus
  "*s++ == c;"
  (== (deref* (cl:prog1 s (= s (+ s 1)))) c))

(reader-test compare-plusplus1
  "*s++ == --c;"
  (== (deref* (cl:prog1 s (= s (+ s 1))))
      (= c (- c 1))))

(reader-test compare-plusplus2
  "*s++ == *--c;"
  (== (deref* (cl:prog1 s (= s (+ s 1))))
      (deref* (= c (- c 1)))))

(reader-test compare-plusplus3
  "*s++ == **--c;"
  (== (deref* (cl:prog1 s (= s (+ s 1))))
      (deref* (deref* (= c (- c 1))))))

(reader-test compare-plusplus4
  "*s++ == ~**--c;"
  (== (deref* (cl:prog1 s (= s (+ s 1))))
      (~ (deref* (deref* (= c (- c 1)))))))

(reader-test function-returning-pointer-to-int-forward-decl
  "int *foo();
123;"
  123)

(reader-test x**y
  "x**y;"
  (* x (deref* y)))

(reader-test x+*y
  "x+*y;"
  (+ x (deref* y)))

(reader-test x-*y
  "x-*y;"
  (- x (deref* y)))

(reader-test x-*y
  "x-**y;"
  (- x (deref* (deref* y))))

(reader-test x***y
  "x***y;"
  (* x (deref* (deref* y))))

(reader-test x*-y
  "x*-y;"
  (* x (cl:- y)))

;; (reader-test array-of-array-of-ints
;;   "int foobar[5][5];")

;; (reader-test pointer-to-array-of-ints0
;;   "int (*foobar)[];"
;;   (cl:progn (cl:defparameter foobar 0)))

;; (reader-test pointer-to-array-of-ints1
;;   "int (*foobar)[5];"
;;   (cl:progn (cl:defparameter foobar 0)))

;; (reader-test unclosed-string
;;   "\"foo")

;; Handle empty defines.
(reader-test preprocessor-define-only
  "#define PREPROCESSOR_DEFINE_ONLY
#ifdef PREPROCESSOR_DEFINE_ONLY
int preprocessor_define_only = 1;
#endif
#ifndef PREPROCESSOR_DEFINE_ONLY
int preprocessor_define_only = 2;
#endif"
  (cl:progn (cl:defparameter preprocessor_define_only 1)))

;; Treat line continuation backslash as whitespace.
(reader-test ignore-line-continuation-character
  "int \\
ignore_line_continuation_character = 3;"
  (cl:progn (cl:defparameter ignore_line_continuation_character 3)))

;; Fix parsing of decimal 0
(reader-test parse-decimal-zero
  "#if 0
int preprocessor_else = 1;
#endif")

;; Handle #else, test 1.
(reader-test preprocessor-else
  "#if 0
int preprocessor_else = 1;
#else
int preprocessor_else = 2;
#endif"
  (cl:progn (cl:defparameter preprocessor_else 2)))

;; Handle #else, test 2.
(reader-test preprocessor-parentheses
  "#if (0 && 1)
int preprocessor_parentheses = 1;
#else
int preprocessor_parentheses = 2;
#endif"
  (cl:progn (cl:defparameter preprocessor_parentheses 2)))

;; Handle #else, test 3.
(reader-test preprocessor-not
  "#if (!0 && 1)
int preprocessor_not = 1;
#else
int preprocessor_not = 2;
#endif"
  (cl:progn (cl:defparameter preprocessor_not 1)))

;; Handle defined operator, test 1.
(reader-test preprocessor-defined-false
  "#if defined PREPROCESSOR_DEFINED_FALSE
int preprocessor_defined_false = 1;
#else
int preprocessor_defined_false = 2;
#endif"
  (cl:progn (cl:defparameter preprocessor_defined_false 2)))

;; Handle defined operator, test 2.
(reader-test preprocessor-not-defined
  "#if ! defined PREPROCESSOR_NOT_DEFINED
int preprocessor_not_defined = 1;
#else
int preprocessor_not_defined = 2;
#endif"
  (cl:progn (cl:defparameter preprocessor_not_defined 1)))

;; Handle defined operator, test 3.
(reader-test preprocessor-defined-true
  "#define PREPROCESSOR_DEFINED_TRUE
#if defined PREPROCESSOR_DEFINED_TRUE
int preprocessor_defined_true = 1;
#else
int preprocessor_defined_true = 2;
#endif"
  (cl:progn (cl:defparameter preprocessor_defined_true 1)))

;; Handle defined operator, test 4.
(reader-test preprocessor-defined-complex
  "#define SOMETHING 0
#define PREPROCESSOR_DEFINED_COMPLEX
#if (defined PREPROCESSOR_DEFINED_COMPLEX && ! defined NOT_DEFINED && SOMETHING)
int preprocessor_defined_complex = 1;
#else
int preprocessor_defined_complex = 2;
#endif"
  (cl:progn (cl:defparameter preprocessor_defined_complex 2)))

;; Handle line continuation in preprocessor.
(reader-test preprocessor-defined-complex-wrapped
  "#define SOMETHING 3
#define PREPROCESSOR_DEFINED_COMPLEX_WRAPPED
#if (defined PREPROCESSOR_DEFINED_COMPLEX_WRAPPED \\
     && ! defined NOT_DEFINED && SOMETHING)
int preprocessor_defined_complex_wrapped = 1;
#else
int preprocessor_defined_complex_wrapped = 2;
#endif"
  (cl:progn (cl:defparameter preprocessor_defined_complex_wrapped 1)))

;; Support using undefined macros as values.
(reader-test preprocessor-use-undefined-macro
  "#if PREPROCESSOR_USE_UNDEFINED_MACRO
int preprocessor_use_undefined_macro = 1;
#else
int preprocessor_use_undefined_macro = 2;
#endif"
  (cl:progn (cl:defparameter preprocessor_use_undefined_macro 2)))

;; Macro with one argument.
(reader-test preprocessor-macro-with-one-argument
  "#define test 3
#define MACRO_WITH_ONE_ARGUMENT(argument) argument
int preprocessor_macro_with_one_argument = MACRO_WITH_ONE_ARGUMENT(test);"
  (cl:progn (cl:defparameter preprocessor_macro_with_one_argument 3)))

;; Macro with no concatenation.
;; Note: gcc produces a failure for this fragment:
;; error: ‘__argument’ undeclared here (not in a function)
(reader-test preprocessor-no-concatenation
  "#define __test 3
#define MACRO_NO_CONCATENATION(argument) __argument
int preprocessor_macro_no_concatenation = MACRO_NO_CONCATENATION(test);"
  (cl:progn (cl:defparameter preprocessor_macro_no_concatenation 3)))

;; Macro with concatenation.
(reader-test preprocessor-macro-with-concatenation
  "#define __test 3
#define MACRO_WITH_CONCATENATION(argument) __##argument
int preprocessor_macro_with_concatenation = MACRO_WITH_CONCATENATION(test);"
  (cl:progn (cl:defparameter preprocessor_macro_with_concatenation 3)))

;; Macro with double concatenation.
(reader-test preprocessor-double-concatenation
  "#define __test__ 4
#define DOUBLE_CONCATENATION(argument) __##argument##__
int preprocessor_double_concatenation = DOUBLE_CONCATENATION(test);"
  (cl:progn (cl:defparameter preprocessor_double_concatenation 4)))

;; Preprocessor #elif.
(reader-test preprocessor-elif
  "#if 0
# define _GL_ATTRIBUTE_DEPRECATED 3
#elif 3
# define _GL_ATTRIBUTE_DEPRECATED 5
#endif
int preprocessor_elif = _GL_ATTRIBUTE_DEPRECATED;"
  (cl:progn (cl:defparameter preprocessor_elif 5)))

;; Preprocessor indentation after #.
(reader-test preprocessor-indentation
  "# if 0
# define _GL_ATTRIBUTE_DEPRECATED 3
# elif 0
# define _GL_ATTRIBUTE_DEPRECATED 5
# else
# define _GL_ATTRIBUTE_DEPRECATED 8
# endif
int preprocessor_indentation = _GL_ATTRIBUTE_DEPRECATED;"
  (cl:progn (cl:defparameter preprocessor_indentation 8)))

(reader-test preprocessor-long
  "# define PREPROCESSOR_LONG 33L
int preprocessor_long = PREPROCESSOR_LONG;"
  (cl:progn (cl:defparameter preprocessor_long 33)))

(reader-test preprocessor-long-lowercase
  "# define PREPROCESSOR_LONG 33l
int preprocessor_long = PREPROCESSOR_LONG;"
  (cl:progn (cl:defparameter preprocessor_long 33)))

(reader-test preprocessor-nested
  "#define PREPROCESSOR_NESTED_DEFINED
#if 0
# if 0
   int preprocessor_nested = 1;
# else
#  ifdef PREPROCESSOR_NESTED_DEFINED
    int preprocessor_nested = 4;
#  else
    int preprocessor_nested = 3;
#  endif
# endif
#else
# ifndef PREPROCESSOR_NESTED_NOT_DEFINED
#  if 0
    int preprocessor_nested = 2;
#  else
    int preprocessor_nested = 9;
#  endif
# else
   int preprocessor_nested = 8;
# endif
#endif"
  (cl:progn (cl:defparameter preprocessor_nested 9)))

(reader-test preprocessor-defined-value
  "#define PREPROCESSOR_DEFINED_VALUE 1
#if defined PREPROCESSOR_DEFINED_VALUE || defined PREPROCESSOR_UNDEFINED_VALUE
int preprocessor_defined_value = 3;
#else
int preprocessor_defined_value = 4;
#endif"
  (cl:progn (cl:defparameter preprocessor_defined_value 3)))

(reader-test preprocessor-empty-define-only
  "#define PREPROCESSOR_EMPTY_DEFINE_ONLY
")

(reader-test preprocessor-use-empty-define
  "#define PREPROCESSOR_USE_EMPTY_DEFINE
PREPROCESSOR_USE_EMPTY_DEFINE
")

(reader-test preprocessor-use-empty-define-own-line
  "#define PREPROCESSOR_USE_EMPTY_DEFINE_OWN_LINE
int use_empty_define_own_line_1 = 8;
PREPROCESSOR_USE_EMPTY_DEFINE_OWN_LINE
#if 1
int use_empty_define_own_line_2 = 3;
#endif"
  (cl:progn (cl:defparameter use_empty_define_own_line_1 8))
  (cl:progn (cl:defparameter use_empty_define_own_line_2 3)))

(reader-test preprocessor-inline-comment
  "#define _GL_FLOAT_EXPONENT_STRLEN_BOUND(min, max)  \\
  (      -100 < (min) && (max) <     100 ? 3       \\
   :    -1000 < (min) && (max) <    1000 ? 4       \\
   :   -10000 < (min) && (max) <   10000 ? 5       \\
   :  -100000 < (min) && (max) <  100000 ? 6       \\
   : -1000000 < (min) && (max) < 1000000 ? 7       \\
   : 8 /* not a tight bound */)
int preprocessor_inline_comment = 1;"
  (cl:progn (cl:defparameter preprocessor_inline_comment 1)))

(reader-test preprocessor-character-backslash
  "#if ('\\\\' == 92)
int preprocessor_character_backslash = 1;
#endif"
  (cl:progn (cl:defparameter preprocessor_character_backslash 1)))
