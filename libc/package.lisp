(in-package #:vacietis)
(named-readtables:in-readtable vacietis:vacietis)

(defpackage #:vacietis.libc.errno.h
  (:use)
  (:export #:errno))

(defpackage #:vacietis.libc.stddef.h
  (:use #:named-readtables #:vacietis #:cl)
  (:export
   #:NULL
   #:offsetof))

(defpackage #:vacietis.libc.math.h
  (:use #:cl #:vacietis #:named-readtables)
  (:import-from #:vacietis.c #:deref*)
  (:export
   #:HUGE_VAL
   #:sin
   #:cos
   #:tan
   #:asin
   #:acos
   #:atan
   #:atan2
   #:sinh
   #:cosh
   #:tanh
   #:sqrt
   #:exp
   #:pow
   #:log
   #:log10
   #:fabs
   #:floor
   #:ceil
   #:fmod
   #:ldexp
   #:frexp
   #:modf))

(defpackage #:vacietis.libc.ctype.h
  (:use #:cl #:named-readtables)
  (:export
   #:isspace
   #:isalnum
   #:isalpha
   #:iscntrl
   #:isdigit
   #:isgraph
   #:islower
   #:isprint
   #:ispunct
   #:isupper
   #:isxdigit
   #:toupper
   #:tolower))

(defpackage #:vacietis.libc.stdio.h
  (:use #:cl #:named-readtables #:vacietis #:vacietis.libc.stddef.h)
  (:import-from #:vacietis.c #:deref*)
  (:shadow #:remove)
  (:export
   #:EOF
   #:stdin
   #:stdout
   #:stderr
   #:clearerr
   #:feof
   #:ferror
   #:perror
   #:fopen
   #:fflush
   #:fclose
   #:freopen
   #:remove
   #:rename
   #:tmpfile
   #:tmpnam
   #:setvbuf
   #:fgetc
   #:fputc
   #:fgets
   #:gets
   #:fputs
   #:ungetc
   #:fread
   #:fwrite
   #:SEEK_SET
   #:SEEK_CUR
   #:SEEK_END
   #:fseek
   #:ftell
   #:rewind
   #:fgetpos
   #:fsetpos
   ))

(defpackage #:vacietis.libc.string.h
  (:use #:cl #:vacietis #:named-readtables #:vacietis.libc.stddef.h)
  (:export
   #:strerror))

(defpackage #:vacietis.libc.stdlib.h
  (:use #:cl #:vacietis #:named-readtables
        #:vacietis.libc.errno.h #:vacietis.libc.stddef.h)
  (:import-from #:vacietis.c #:deref*)
  (:shadow #:abort)
  (:export
   #:malloc
   #:calloc
   #:realloc
   #:free
   #:RAND_MAX
   #:rand
   #:srand
   #:atoi
   #:atol
   #:atoll
   #:atof
   #:strtod
   #:strtof
   #:strtol
   #:strtoll
   #:strtoul
   #:strtoull
   #:EXIT_SUCCESS
   #:EXIT_FAILURE
   #:abort
   #:exit
   #:atexit
   #:getenv
   #:setenv
   #:system
   #:abs
   #:labs
   #:bsearch
   #:qsort
   #:div
   #:ldiv
   ))
