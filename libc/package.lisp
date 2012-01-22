(in-package #:vacietis)
(in-readtable vacietis)

(defpackage #:vacietis.math
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

(defpackage #:vacietis.ctype
  (:use #:cl)
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

(defpackage #:vacietis.errno
  (:use)
  (:export #:errno))

(defpackage #:vacieitis.stddef
  (:use #:named-readtables #:vacietis)
  (:export
   #:NULL
   #:offsetof))

(defpackage #:vacietis.stdio
  (:use #:cl #:named-readtables #:vacietis)
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

(defpackage #:vacietis.string
  (:use #:cl #:vacietis)
  (:export
   #:strerror))
