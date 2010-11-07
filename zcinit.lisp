; You must load this file before either loading the Zeta-C system itself or loading
; or editing any files of Zeta-C code.

#-Genera
(when (not (intern-soft "GMAP" 'global))
  (load "zeta-c: library; gmap"))

(defpackage #-MIT C #+MIT 'C				   ; bug in MIT system.
  (:use)
  (:relative-names ("GL" GLOBAL))
  (:size 128)
  (:export "+" "-" "*" "//" "%" "<<" ">>" "&" "|" "^" "~"
		 "==" "!=" "<" ">" "<=" ">=" "!" "&&" "||"
		 "=" "++X" "X++" "--X" "X--"
		 "++" "--"								    ; For the lexer.
		 "+=" "-=" "*=" "//=" "%=" "&=" "|=" "^=" "<<=" ">>="
		 "FUNCALL+" "FCN+" "[]" "." "->" "?:" "PROGN+"
		 "if" "else" "BLOCK+" "goto" "LABEL+"
		 "while" "do" "for" "break" "continue" "return"
		 "switch" "case" "default" "setjmp"
		 "DEFUNC+" "DECL+"
		 "char" "int" "long" "short" "signed" "unsigned"
		 "float" "double" "void" "lispval"
		 "struct" "packed_struct" "union" "enum" "..."
		 "extern" "static" "register" "auto" "typedef" "restarg" "optarg"
		 "CAST+" "sizeof"
		 "#LISP" "LIST+" "QUOTE+" "STRING+"))

; Sigh, incompatibility runs rampant.
(defconst *rubout-handler-message* #+Symbolics ':input-editor
							#-Symbolics ':rubout-handler)


; ================================================================
; Customization variables.

(defvar *firstclass-structures* t
  "Controls whether structures are firstclass objects, i.e., whether they can
   be targets of assignments, passed as arguments, or returned as function values.
   If NIL, any attempt to use structures in one of these ways will signal an error.
   (Note that we're talking about structures themselves, not just pointers to
   structures, which are always firstclass objects.)  Firstclass structures are
   supported by several of the Unix C compilers, but by very few of the non-Unix
   compilers, so for maximum portability they should not be used.")

(defvar *compare-incomparable-pointers* nil
  "If two pointers point into different arrays, then in a strict sense they are not
   comparable, since their relative values have only to do with accidents in memory
   allocation.  So ZETA-C normally checks for attempts to compare two such pointers,
   and on finding one, issues an error.  If this variable is T, however, ZETA-C will
   ignore the arrays of the pointers and compare only the indices (giving better
   performance at the expense of possible anomalous behavior).
/The variable takes effect at both compile time and run time: if it's NIL at compile
   time, a run time check is compiled; if it's T at compile time, no check is
   compiled (so code compiled this way ignores the runtime value).  To turn it on at
   compile time, say in your C source file
     #define ZETA_C_COMPARE_INCOMPARABLE_POINTERS
   (or /"#undef .../" to turn it off).")

(defvar *system-include-directory* "ZETA-C:Include;"
  "The directory in which an #included file is looked for when the name is
   delimited with angle brackets, as in /"#include <stdio.h>/"; analogous to
   //usr//include on Unix.")

(defvar *system-include-sys-directory* #-TI "ZETA-C:Include;Sys;"
							    #+TI "ZETA-C:Include.Sys;"	  ;
  "The directory in which to look for a file included by a command of the form
   /"#include <sys/foobar.h>/".  See *SYSTEM-INCLUDE-DIRECTORY*.")

(defvar *comments-nest* nil
  "Controls what happens if /"//*/" is found inside a comment: if NIL (the C
   standard), it is ignored; if T, it is considered to open a nested comment.")

(defvar *suppress-auto-float-to-double* nil
  "Controls whether floating-point arithmetic is forced to be done in double-
   precision.  If T, it is not; single-precision operands will produce a single-
   precision result (though variables declared /"double/" will still be double-
   precision, and operations where either operand is double will be done in
   double-precision).  NIL (the default) forces double-precision to be used
   everywhere, as is traditional in C implementations.  This variable takes effect
   at compile time, not run time; to turn it on, say in your C source file
      #define ZETA_C_SUPPRESS_AUTO_FLOAT_TO_DOUBLE
   (or /"#undef .../" to turn it off).")

(defvar *case-insensitive* nil
  "If T, case is ignored in variable and function names.  The default is NIL, which
   gives standard C behavior.")


; ================================================================
; Program setup.

; MIT System 98 and Symbolics 5.0 have new package systems, which appear to even be
; compatible!
(defun create-c-package (name)
  "Creates a package for a C program to live in.  Be sure you don't use any names
   of existing LispM system packages, since the new package will shadow the old
   one."
  (let ((name (string-upcase name)))
    (or (pkg-find-package name :find)
	   (make-package name :size 256. :use '(C)
				  :relative-names '(("GL" GLOBAL))))))

(defvar *c-package* (pkg-find-package "C")
  "A variable holding the C package object, for convenience.")

(defun c-package-p (name)
  "Is this a C program package, that is, a subpackage of C:?"
  (let ((pkg (pkg-find-package name ':find)))
    (and pkg (eq (car (package-use-list pkg)) *c-package*))))

(defvar *c-user-package* (create-c-package "C-USER"))

(defvar *package-variable-initialization-alist* nil
  "An alist associating C packages with alists associating variables with
   expressions for computing their initial values.")


; ================================================================
; Proto-declarations (must be here because of file loading order).

; From ZCCLIB.C, String manipulation section.
(export 'c:(|strcpy| |strncpy| |strcat| |strncat|
		  |strchr| |strcmp| |strncmp| |strlen| |strcspn|) 'c)
(export 'c:(|memcpy| |memmove| |memset| |memcmp| |memchr|) 'c)

; From ZCCLIB.C, Memory allocation section.
(export 'c:(|malloc| |calloc| |realloc|) 'c)

; From ZCCLIB.C, Character type table section.
(export 'c:(|_ctype_| |toupper| |tolower|) 'c)

; From ZCCLIB.C, Standard I/O section.
(export 'c:(|scanf| |sscanf| |fscanf|) 'c)

; From ZCCLIB.C, Imitation Unix section.
(export 'c:(|times|) 'c)

; From ZCLIB.LISP, Standard I/O section.
(export 'c:(|fgetc|) 'c)

; ================================================================
; Some areas to cons in.  These are in order of increasing permanence.

;; Temporary areas cause too many gross bugs; and the machines all have, or soon
;; will have, ephemeral GC or equivalent.
(defvar zc-temporary-area working-storage-area
  "The area Zeta-C uses for consing of stuff whose lifetime is the compilation
   of one top-level form.")

(defvar zc-runtime-consing-area
	   (make-area ':name 'zc-runtime-consing-area
			    ':gc ':dynamic
			    ':room t)
  "The area compiled Zeta-C code uses for consing pointers and arrays.  At some
   point this stuff will probably get freelisted, but for now we just let it be
   dynamic.")

(defvar zc-working-area working-storage-area
  "The area Zeta-C uses for consing of stuff that lasts longer than the compilation
   of one top-level form, but is not likely to be around indefinitely.")

(defvar zc-permanent-area permanent-storage-area
  "The area Zeta-C uses for consing of stuff that will probably not need to be
   garbage collected.")

; Fix bug in package creation *before* creating cparse and zclib
#+TI (load "zeta-c:source;explorer-2-patches")
(create-c-package 'cparse)
(create-c-package 'zclib)

; End of zcinit.lisp
