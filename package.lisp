(in-package #:cl)

(defpackage #:vacietis
  (:use #:cl #:named-readtables #:anaphora)
  (:export
   #:vacietis
   #:c-readtable
   #:string-to-char*))

(in-package #:vacietis)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defreadtable vacietis ;; defreadtable isn't eval-whened, grrr
    (:merge :standard)
    (:case :invert)))

(in-readtable vacietis)

(defpackage #:vacietis.c
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

   ;; stuff that should be in header files
   #:NULL

   ;; stuff we define
   #:deref*
   #:mkptr&
   #:post--
   #:post++
   #:[]
   ))

(defpackage #:vacietis.reader
  (:use #:cl #:named-readtables #:vacietis #:anaphora))
