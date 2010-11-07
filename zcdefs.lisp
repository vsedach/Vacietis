;;; This file contains the system definition for ZETA-C.
;;; This is the Symbolics Genera 7 version.

;;; Compatibility.  Oboy
;;; A feature for Symbolics Genera 7...
#+(and Symbolics Row-major (not Genera)) (push ':Genera cl:*features*)
;;; ... and one for character objects (also in Explorer Rel 3).
#+(and Genera (not Chars)) (push ':Chars cl:*features*)
#+(and TI ??? (not Chars)) (push ':Chars cl:*features*)

(defpackage #+MIT 'ZETA-C #-MIT ZETA-C (:use global))

;;; This stuff has already been set up in Genera 7.2.
(unless (assq :c fs:*default-canonical-types*)
  (fs:define-canonical-type :c "C")
  (fs:define-canonical-type :c-include "H"))

;;; I think this had some desirable effect, which I don't really understand, on the
;;; patch system.  Unfortunately it also screws up Edit Compiler Warnings.  Sigh.
;;; #+Symbolics (push ':c fs:*known-types*)

;;; Then again, maybe this is what makes the patch system happy.
(push ':c sct:*source-file-types*)


;;; ================================================================
;;; Definitions for Symbolics Genera 7.

#+Genera	; Already present in 7.2.  Override.
(let ((inhibit-fdefine-warnings t))
  (sct:define-module-type :c :c :bin
    sct:bin-load-module))

#+Genera
(defsystem ZETA-C
    (:pretty-name "ZETA-C (c) 1987 ZETA-SOFT, Ltd."
     :default-pathname "ZETA-C: Source;"
     :patchable t :journal-directory "ZETA-C: Patch;"
     :bug-reports ("Bug-ZETA-C" "Report problems with ZETA-C.")
     :advertised-in (:herald :finger :disk-label)
     :maintaining-sites (:ZETA-SOFT)
     :source-category :restricted
     :distribute-binaries t)
  (:module init "zcinit" (:type :lisp-read-only) (:source-category :basic))
  (:module system-patches "symbolics-7-2-patches" (:source-category :basic))
  (:module gmapm "ZETA-C:Library;gmap" (:source-category :basic))
  (:module zcmac "zcmac")
  (:module zcprim "zcprim" (:uses-definitions-from zcmac))
  ;; If the parser isn't loaded, we can't parse it!  So we keep preparsed
  ;; versions around.
  (:module preparsed-parser ("ytab-parsed" "zctoken-parsed") (:root-module nil)
	   (:distribute-binaries nil))
  (:module c-parser ("ytab" "zctoken") (:type :c)
	   (:in-order-to :compile (:load preparsed-parser)))
  (:module zcclib "zcclib" (:type :c) (:source-category :basic))
  (:module user-stuff ("zcuser" "zczwei-sym") (:source-category :basic))
  (:serial (:parallel init system-patches gmapm) "zcmisc" "zctype" zcmac zcprim
	   "zcparse" "zclib" c-parser (:parallel user-stuff zcclib))
  ;; These are here only for the distribution dumper.
  ;; Alas, (:distribute-binaries nil) doesn't override the default.  Pain.
  (:module sysdefs ("SYS:Site;zeta-c.system" "SYS:Site;zeta-c-runtime.system")
	   (:root-module nil) (:source-category :basic) (:distribute-binaries nil))
  (:module turtle "turtle" (:type :c) (:root-module nil) (:source-category :basic)
	   (:distribute-binaries nil))
  (:module includes ("ZETA-C:Include;ctype.h" "ZETA-C:Include;errno.h"
		     "ZETA-C:Include;math.h"
		     "ZETA-C:Include;setjmp.h" "ZETA-C:Include;sgtty.h"
		     "ZETA-C:Include;signal.h" "ZETA-C:Include;stdio.h"
		     "ZETA-C:Include;stdlib.h" "ZETA-C:Include;string.h"
		     "ZETA-C:Include;strings.h"
		     "ZETA-C:Include;Sys;stat.h" "ZETA-C:Include;Sys;time.h"
		     "ZETA-C:Include;Sys;times.h" "ZETA-C:Include;Sys;types.h")
	   (:type :c) (:root-module nil) (:source-category :basic)
	   (:distribute-binaries nil)))

#+Genera
(defsystem ZETA-C-RUNTIME
    (:pretty-name "ZETA-C-RUNTIME (c) 1987 ZETA-SOFT, Ltd."
     :default-pathname "ZETA-C: Source;"
     :source-category :restricted
     :distribute-binaries t)
  (:module zeta-c-module zeta-c (:type :system) (:root-module nil))
  (:module init "zcinit" (:type :lisp-read-only) (:source-category :basic))
  (:module zcrun "zcrun" (:in-order-to :compile (load zeta-c-module)))
  (:module zclib "zclib")
  (:module zcclib "zcclib" (:type :c)))

#+Genera
(defmethod (:compile c-module) (system-op &rest keys &key recompile no-compile
					  &allow-other-keys)
  (unless no-compile
    (when (eq system-op :compile)
      (lexpr-funcall #'sct:default-compile
		     self system-op recompile
		     (lambda (source bin module &rest ignore)
		       (let-if (sct:system-default-package sct:*system*)
			       ((package (pkg-find-package
					   (sct:system-default-package sct:*system*))))
			 (zeta-c:c-compile-file source bin (sct:package-for-module module))))
		     '("C-compile" "C-compiling" "C-compiled")
		     keys))))

;;; ================================================================
;;; Definitions for Symbolics Rel 6 and Explorer.

#-Genera
(si:define-simple-transformation :c-compile
				 #+Symbolics zeta-c:c-compile-file
				 #-Symbolics zeta-c:cc-file-1
  si:file-newer-than-file-p
  (:c)
  (#+3600 :bin #+Explorer :xfasl #+Lambda :qfasl #+CADR :qfasl)
  ("C-compile" "C-compiling" "C-compiled"))

#-Genera
(defmacro (:c-compile-load si:defsystem-macro)
	  (input &optional com-dep load-dep com-cond load-cond)
  `(:fasload (:c-compile ,input ,com-dep ,com-cond)
	     ,load-dep ,load-cond))

#-Genera 
(defsystem ZETA-C
  (:name "Zeta-C")
  (:pathname-default "ZETA-C: Source;")
  (:patchable "ZETA-C: Patch;")
  #+Symbolics (:maintaining-sites :ZETA-SOFT)
  (:module init "zcinit")
  (:module zcmisc "zcmisc")
  (:module zctype "zctype")
  (:module zcmac "zcmac")
  (:module zcprim "zcprim")
  (:module zclib "zclib")
  (:module zcparse "zcparse")
  ;; If the parser isn't loaded, we can't parse it!  So we keep preparsed
  ;; versions around.
  (:module ytab-parsed "ytab-parsed")
  (:module zctoken-parsed "zctoken-parsed")
  (:module ytab "ytab")
  (:module zctoken "zctoken")
  (:module zcclib "zcclib")
  (:module zczwei #+Symbolics "zczwei-sym" #-Symbolics "zczwei-lmiti")
  (:module zcuser "zcuser")
  (:module system-patches #+Symbolics "symbolics-6-patches"
			  #+TI "explorer-2-patches"
			  #+Lambda "lambda-102-patches"	; "LMI" and "MIT" not
			  #+CADR "system-99-patches")	; distinguishable, sigh
  (:readfile init)
  (:compile-load system-patches)
  (:compile-load zcmisc ((:readfile init) (:fasload system-patches))
			((:readfile init) (:fasload system-patches)))
  (:compile-load zctype ((:readfile init) (:fasload zcmisc)) (:readfile init))
  (:compile-load zcmac ((:readfile init) (:fasload zcmisc zctype))
		       (:readfile init))
  (:compile-load zcprim ((:readfile init) (:fasload zcmisc zctype zcmac))
			((:readfile init) (:fasload zctype)))
  (:compile-load zclib ((:readfile init) (:fasload zcmisc zcprim))
		      (:readfile init))
  (:compile-load zcparse ((:readfile init) (:fasload zcmisc zclib))
			 ((:readfile init) (:fasload zclib)))
  (:compile-load zcuser ((:readfile init) (:fasload zclib))
			(:readfile init))
  (:compile zctoken-parsed ((:readfile init)
			    (:fasload zcmisc zctype zcmac zcprim zclib
				      zcparse zcuser)))
  (:skip :fasload zctoken-parsed ((:readfile init) (:fasload zclib)))
  (:compile ytab-parsed ((:readfile init)
			 (:fasload zcmisc zctype zcmac zcprim zclib
				   zcparse zcuser)))
  (:skip :fasload ytab-parsed ((:readfile init) (:fasload zclib)))
  (:c-compile-load zctoken ((:compile ytab-parsed zctoken-parsed)
			    (:fasload zcmisc zctype zcmac zcprim zclib zcparse
				      zcuser ytab-parsed zctoken-parsed))
			 ((:readfile init) (:fasload zclib zcparse)))
  (:c-compile-load ytab ((:compile ytab-parsed zctoken-parsed)
			 (:fasload zcmisc zctype zcmac zcprim zclib zcparse zcuser
				   ytab-parsed zctoken-parsed))
			((:readfile init) (:fasload zclib zctoken)))
  ;; Annoyance: this file cannot be compiled until it has been loaded, because the
  ;; parser calls some routines in it (e.g., strlen).  We'll need to make a -parsed
  ;; version for initial ports, but I'm not going to bother putting it into the
  ;; system definition, because the previous compiled version will almost always be
  ;; all we need.
  (:c-compile-load zcclib ((:c-compile ytab zctoken)
			  (:fasload zcmisc zctype zcmac zcprim zclib zcparse zcuser
				    ytab zctoken))
			 ((:readfile init) (:fasload zclib ytab)))
  (:compile-load zczwei (:readfile init) (:readfile init)))

#+(and Symbolics (not Genera))
(dis:defpseudo-system ZETA-C-MISC
  ;; Glue.
  (:files "Sys:Site;zeta-c.system.newest")
  (:files "Sys:Site;zeta-c-runtime.system.newest")
  ;; The standard partial source.
  (:files "Zeta-C:Source;zcinit.lisp.newest" :source-file)
  (:files "Zeta-C:Source;zclib.lisp.newest" :source-file)
  (:files "Zeta-C:Source;zcclib.c.newest" :source-file)
  (:files "Zeta-C:Source;zczwei.lisp.newest" :source-file)
  (:files "Zeta-C:Source;zcuser.lisp.newest" :source-file)
  (:files "Zeta-C:Source;symbolics-6-patches.lisp.newest" :source-file)
  ;; The runtime kernel.
  (:files "Zeta-C:Source;zcrun.lisp.newest" :source-file)
  (:files "Zeta-C:Source;zcrun.bin.newest" :object-file)
  ;; GMAP.
  (:files "Zeta-C:Library;gmap.lisp.newest" :source-file)
  (:files "Zeta-C:Library;gmap.bin.newest" :object-file)
  ;; Standard header files.
  (:files "Zeta-C:Include;*.h.newest")
  (:files "Zeta-C:Include;Sys;*.h.newest")
  ;; A bit of sample code.
  (:files "Zeta-C:Source;turtle.c.newest"))

;; Don't try to MAKE-SYSTEM :COMPILE this without loading ZETA-C first.
#-Genera
(defsystem ZETA-C-RUNTIME
  (:name "Zeta-C Runtime")
  (:pathname-default "ZETA-C: Source;")
  (:module init "zcinit")
  (:module zcrun "zcrun")
  (:module zclib "zclib")
  (:module zcclib "zcclib")
  (:readfile init)
  (:compile-load zcrun (:readfile init) (:readfile init))
  (:compile-load zclib (:readfile init) (:readfile init))
  (:c-compile-load zcclib (:readfile init) (:readfile init)))

#+(and Symbolics (not Genera))
(dis:defpseudo-system ZETA-C-RUNTIME-DIST
  (:files "Sys:Site;zeta-c-runtime.system.newest")
  (:files "Zeta-C:Source;zcinit.lisp.newest" :source-file)
  (:files "Zeta-C:Source;zcrun.bin.newest" :object-file)
  (:files "Zeta-C:Source;zclib.bin.newest" :object-file)
  (:files "Zeta-C:Source;zcclib.bin.newest" :object-file))


; End of ZCDEFS.LISP
