(in-package #:vacietis)
(in-readtable vacietis)

(defpackage #:vacietis.reader
  (:use #:cl #:named-readtables #:vacietis #:anaphora)
  (:import-from #:vacietis
                #:*basic-c-types*
                #:*type-qualifiers*
                #:*variable-sizes*
                #:*c-structs*
                #:literal
                #:include-libc-file))
