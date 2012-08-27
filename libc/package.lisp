(in-package #:vacietis)
(named-readtables:in-readtable vacietis:vacietis)

(defpackage #:vacietis.libc.errno.h
  (:use #:cl #:named-readtables #:vacietis)
  (:export
   #:errno
   #:EPERM
   #:ENOENT
   #:ESRCH
   #:EINTR
   #:EIO
   #:ENXIO
   #:E2BIG
   #:ENOEXEC
   #:EBADF
   #:ECHILD
   #:EAGAIN
   #:ENOMEM
   #:EACCES
   #:EFAULT
   #:ENOTBLK
   #:EBUSY
   #:EEXIST
   #:EXDEV
   #:ENODEV
   #:ENOTDIR
   #:EISDIR
   #:EINVAL
   #:ENFILE
   #:EMFILE
   #:ENOTTY
   #:ETXTBSY
   #:EFBIG
   #:ENOSPC
   #:ESPIPE
   #:EROFS
   #:EMLINK
   #:EPIPE
   #:EDOM
   #:ERANGE
   ))

(defpackage #:vacietis.libc.stddef.h
  (:use #:cl #:named-readtables #:vacietis)
  (:export
   #:NULL
   #:offsetof))

(defmacro deflibcpkg (name &rest other-opts)
  `(defpackage ,name
     (:use #:cl #:named-readtables #:vacietis
           #:vacietis.libc.errno.h #:vacietis.libc.stddef.h)
     (:import-from #:vacietis.c #:deref*)
     ,@other-opts))

(deflibcpkg #:vacietis.libc.math.h
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

(deflibcpkg #:vacietis.libc.ctype.h
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

(deflibcpkg #:vacietis.libc.string.h
  (:export
   #:strerror
   #:strrchr
   #:strspn
   #:strpbrk
   #:strstr
   #:strtok
   #:strtok_r
   #:strcpy
   #:strncpy
   #:strcat
   #:strncat
   #:strchr
   #:strcmp
   #:strncmp
   #:strlen
   #:strcspn
   #:memcpy
   #:memmove
   #:memset
   #:memchr
   #:memcmp))

(deflibcpkg #:vacietis.libc.stdio.h
  (:shadow #:remove)
  (:import-from #:vacietis.libc.string.h #:strerror)
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
   #:setbuf
   #:setvbuf
   #:fgetc
   #:fputc
   #:putc
   #:putchar
   #:fgets
   #:gets
   #:fputs
   #:puts
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
   #:fprintf
   #:printf
   #:sprintf
   ))

(deflibcpkg #:vacietis.libc.stdlib.h
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
