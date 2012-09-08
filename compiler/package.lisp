(in-package #:cl)

(defpackage #:vacietis
  (:use #:cl #:named-readtables #:anaphora)
  (:export
   ;; readtables
   #:vacietis
   #:c-readtable

   ;; memory stuff
   #:size-of
   #:allocate-memory
   #:memptr-mem
   #:memptr-ptr
   #:copy-memptr

   ;; utilities
   #:string-to-char*
   #:char*-to-string

   ;; runtime
   #:*compiler-state*
   #:make-compiler-state
   #:load-c-file
   #:run-c-program
   )
  (:intern
   #:defun/1
   #:compiler-state-pp
   #:compiler-state-typedefs
   #:compiler-state-structs
   #:compiler-state-var-sizes))

(in-package #:vacietis)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defreadtable vacietis ;; defreadtable isn't eval-whened, grrr
    (:merge :standard)
    (:case :invert)))

(in-readtable vacietis)

(defpackage #:vacietis.reader
  (:use #:cl #:named-readtables #:vacietis #:anaphora)
  (:import-from #:vacietis
    #:defun/1
    #:compiler-state-pp
    #:compiler-state-typedefs
    #:compiler-state-structs
    #:compiler-state-var-sizes))

(defpackage #:vacietis.c
  (:use)
  (:export
   ;; operators
   #:=
   #:+=
   #:-=
   #:*=
   #:/=
   #:%=
   #:<<=
   #:>>=
   #:&=
   #:^=
   #:|\|=|
   #:?
   #:|:|
   #:|\|\||
   #:&&
   #:|\||
   #:^
   #:&
   #:==
   #:!=
   #:<
   #:>
   #:<=
   #:>=
   #:<<
   #:>>
   #:++
   #:--
   #:+
   #:-
   #:*
   #:/
   #:%
   #:!
   #:~
   #:->
   #:|.|
   #:?
   #:|:|
   #:|,|

   ;; keywords
   #:auto
   #:break
   #:case
   #:char
   #:const
   #:continue
   #:default
   #:do
   #:double
   #:else
   #:enum
   #:extern
   #:float
   #:for
   #:goto
   #:if
   #:inline
   #:int
   #:long
   #:register
   #:restrict
   #:return
   #:short
   #:signed
   #:sizeof
   #:static
   #:struct
   #:switch
   #:typedef
   #:union
   #:unsigned
   #:void
   #:volatile
   #:while
   #:_Bool
   #:_Complex
   #:_Imaginary

   ;; preprocessor
   #:define
   #:undef
   #:include
   #:if
   #:ifdef
   #:ifndef
   #:else
   #:endif
   #:line
   #:elif
   #:pragma
   #:error

   ;; stuff we define
   #:deref*
   #:mkptr&
   #:post--
   #:post++
   #:[]
   #:|...|
   ))
