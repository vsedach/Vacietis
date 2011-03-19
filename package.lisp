(in-package #:cl)

(defpackage #:vacietis
  (:use #:cl #:named-readtables)
  (:export
   #:vacietis
   #:c-readtable))

(in-package #:vacietis)

(defreadtable vacietis
  (:merge :standard)
  (:case :invert))

(in-readtable vacietis)

(defpackage #:vacietis.c.operators
  (:export #:--))

(defpackage #:vacietis.c.keywords
  (:export #:auto
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
           #:_Imaginary))

(defpackage #:vacietis.reader
  (:use #:cl #:named-readtables #:vacietis #:vacietis.c.operators #:vacietis.c.keywords))
