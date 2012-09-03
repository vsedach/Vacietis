(in-package #:vacietis)
(in-readtable vacietis)

(defpackage #:vacietis.reader
  (:use #:cl #:named-readtables #:vacietis #:anaphora)
  (:import-from #:vacietis
    #:compiler-state-pp
    #:compiler-state-typedefs
    #:compiler-state-structs
    #:compiler-state-var-sizes
    #:include-libc-file))
