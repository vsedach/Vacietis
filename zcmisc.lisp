; This file contains miscellaneous macros and useful functions for Zeta-C.


(defmacro set-in-alist (alist var val)
  "(Macro) Binds VAR to VAL in ALIST, creating the association if necessary."
  `(let ((x (assq ,var ,alist)))
     (if x (rplacd x ,val) (push (cons ,var ,val) ,alist))))

(defmacro tail-push (item list)
  "/"Push/" an item onto the end of a list (rather than the beginning.)"
  `(setf ,list (nconc ,list (ncons ,item))))

(defun cons-if-non-nil (obj list)
  "If OBJ is non-nil, conses it onto LIST; otherwise just returns LIST."
  (if obj (cons obj list) list))

(defmacro push-if-non-nil (obj list)
  "If OBJ is non-nil, pushes it onto LIST."
  `(if ,obj (push ,obj ,list)))

(defun groupn (n list)
  "Takes a list and groups it into sublists of N elements each, e.g.,
   (groupn 3 '(1 2 3 4 5 6 7 8)) => ((1 2 3) (4 5 6) (7 8 nil))"
  (if (zerop n)
	 (ferror "GROUPN called with N = 0")
    (groupn-1 n list)))
(defun groupn-1 (n list)
  (and list (cons (firstn n list) (groupn-1 n (nthcdr n list)))))

(defun group (list pred)
  "Groups the elements of a list into sublists, such that the application of PRED to
   pairs of successive elements of each sublist returns non-NIL.  Does not reorder
   the elements.  Non-destructive; result shares no structure with LIST except the
   elements themselves.  An example:
   (group '((a 1 2) (a 6 7) (b 3 1) (b 6 9) (b -3 0) (a 5 5))
		#'(lambda (x y) (eq (car x) (car y))))
    => (((a 1 2) (a 6 7)) ((b 3 1) (b 6 9) (b -3 0)) ((a 5 5)))"
  (group1 list pred nil nil))
(defun group1 (list pred grp result)
  (cond ((null list)
	    (nreverse (cons-if-non-nil (nreverse grp) result)))
	   ((null grp)
	    (group1 (cdr list) pred (ncons (car list)) result))
	   ((funcall pred (car grp) (car list))
	    (group1 (cdr list) pred (cons (car list) grp) result))
	   (t
	    (group1 list pred nil (cons (nreverse grp) result)))))

(defun nth? (elt list)
  "Returns the 0-origin index of ELT in LIST, or NIL if it's not present."
  (nth?-1 elt list 0))
(defun nth?-1 (elt list i)
  (cond ((null list) nil)
	((eq elt (car list)) i)
	(t (nth?-1 elt (cdr list) (1+ i)))))

(defun nthcar? (x alist)
  "Returns the 0-origin index of the first cons in ALIST whose CAR is X
   or NIL of it's not present."
  (do ((i 0 (1+ i))
	  (alist alist (cdr alist)))
	 ((null alist) nil)
    (when (eq (caar alist) x)
	 (return i))))

; As NTH is to NTHCDR, so FIRST etc. are to ...
(defsubst firstcdr (l)
  (cdr l))

(defsubst secondcdr (l)
  (cddr l))

(defsubst thirdcdr (l)
  (cdddr l))

(defsubst fourthcdr (l)
  (cddddr l))

(defun min-or (&rest values)
  "Computes the MIN of VALUES, except that NILs in VALUES are ignored.  If VALUES
   are all NIL, returns NIL.  Note this is a normal function (all arguments evaluated)."
  (do ((values values (cdr values))
	  (min nil (if (and min (car values)) (min min (car values))
			   (or min (car values)))))
	 ((null values) min)))

(defun revcollapse (fn list)
  "/"Collapses/" a list by applying a binary function to its last two elements, then to
   the previous element and the result, and so on until the beginning of the list is
   reached.  (There must be a better name for this!)"
  #+Symbolics (declare (sys:downward-funarg fn))
  ;; Treat degenerate cases reasonably.
  (cond ((null list) nil)
	   ((null (cdr list)) (car list))
	   ((null (cddr list)) (funcall fn (car list) (cadr list)))
	   (t (funcall fn (car list) (revcollapse fn (cdr list))))))

(defsubst neql (a b)
  (not (eql a b)))

(defun eqv (a b)
  "Logical equivalence predicate, in terms of non-NIL//NIL rather than bitwise."
  (or (and a b) (and (not a) (not b))))

(defun upto-next (number increment)
  "Returns the smallest number greater than or equal to NUMBER which is an integer
   multiple of INCREMENT."
  (* increment (ceiling number increment)))

; I don't believe this doesn't exist.
(defmacro while (pred &body body)
  `(do ()
	  ((not ,pred))
	. ,body))

; This exists in MIT 99; LMI systems?  Not in TI Rel 2.
#+TI
(defmacro unwind-protect-case (ignore body &body clauses)
  "Executes BODY in an UNWIND-PROTECT.  Each of the CLAUSES is of the form
/(:NORMAL <form>) or (:ABORT <form>).  :NORMAL clauses are executed if
the BODY returns normally; :ABORT clauses are executed if the BODY is
thrown out of.  This is only a subset of the Symbolics functionality."
  (let ((abortp-var (gensym)))
    `(let ((,abortp-var t))
       (unwind-protect
	   (prog1 ,body (setq ,abortp-var nil))
	 (if ,abortp-var
	     (progn . ,(cdr (assq ':abort clauses)))
	   . ,(cdr (assq ':normal clauses)))))))

#+Symbolics
(defmacro do-forever (&body body)
  `(do () (()) . ,body))

(defun partition (&functional predicate list)
  "Partitions LIST according to PREDICATE.  Returns two values: a list of those
   elements of LIST that satisfy PREDICATE, and a list of those that don't.  Like
   SUBSET and SUBSET-NOT rolled into one."
  (let ((true-list nil)
	   (false-list nil))
    (do ((list list (cdr list))
	    (true-next (locf true-list))
	    (false-next (locf false-list)))
	   ((null list) (values true-list false-list))
	 (if (funcall predicate (car list))
		(progn (rplacd true-next (ncons (car list)))
			  (setq true-next (cdr true-next)))
	   (rplacd false-next (ncons (car list)))
	   (setq false-next (cdr false-next))))))

(defun list-has-duplicates-p (list)
  "Does LIST have more than one occurrence (by EQ) of any element?  If so, returns
   that element."
  (do ((l list (cdr l)))
	 ((null l) nil)
    (when (memq (car l) (cdr l)) (return (car l)))))

(defun alist-has-duplicates-p (alist)
  "Does ALIST have more than one occurrence of any CAR?  If so, returns that CAR."
  (do ((al alist (cdr al)))
	 ((null al) nil)
    (when (assq (caar al) (cdr al))
	 (return (caar al)))))

; I think this should maybe define interesting proceed-types and such like, but for
; now it's vanilla.
; Also it should be much better at saying what's being compiled.  This will be easy
; when we get the source locator system in.
(defun zcerror (fmt &rest args)
  (declare (special zcprim>*expanding-defunc+*))
  (if zcprim>*expanding-defunc+*
	 (lexpr-funcall #'ferror (string-append "Error while C-compiling ~A:~%" fmt)
				 (cons zcprim>*expanding-defunc+* args))
    (lexpr-funcall #'ferror fmt args)))

(defprop zcerror t :error-reporter)

(defun zcwarn (fmt &rest args)
  (declare (special zcprim>*expanding-defunc+*))
  (let ((func zcprim>*expanding-defunc+*))
    (if func
	   (lexpr-funcall #'compiler:warn `(:function ,func) #-Symbolics nil fmt args)
	 (send error-output ':fresh-line)
	 (lexpr-funcall #'format error-output fmt args))))

(defmacro in-area (area &body body)
  `(let ((default-cons-area ,area))
	. ,body))


; ================================================================
; Pointer abstraction.

(defmacro zcptr>array (ptr)
  "Returns the array part of a consed pointer."
  `(car ,ptr))

(defmacro zcptr>index (ptr)
  "Returns the index part of a consed pointer."
  `(cdr ,ptr))

(defmacro zcptr>cons (array index &optional reuse)
  "Conses a pointer to the INDEXth element of ARRAY.  If REUSE is provided, it is
   a zcptr to be modified to point to the new ARRAY and INDEX; except, if either
   the ARRAY or INDEX expression (not value) is :OLD, that cell of the pointer is
   not changed."
  (if reuse
	 (let ((rplaca-form (if (eq array ':old) reuse `(rplaca ,reuse ,array))))
	   (if (eq index `:old) rplaca-form `(rplacd ,rplaca-form ,index)))
    `(cons ,array ,index)))

(defsubst zcptr>ptr-p (frob)
  "Is FROB a C pointer?"
  (listp frob))

(defsubst zcptr>aref (array index)
  (aref array index))

(defsubst zcptr>aset (value array index)
  (aset value array index))

(#-Symbolics defsetf #+Symbolics cl:defsetf zcptr>aref (array index) (value)
  `(zcptr>aset ,value ,array ,index))

(defmacro zcptr>aref-s8b (array index)
  (zcprim>8-bit-sign-extend `(zcptr>aref ,array ,index)))

(defmacro zcptr>aset-s8b (value array index)
  `(zcptr>aset ,value ,array ,index))

(#-Symbolics defsetf #+Symbolics cl:defsetf zcptr>aref-s8b (array index) (value)
  `(zcptr>aset-s8b ,value ,array ,index))

(defmacro zcptr>aref-s16b (array index)
  (zcprim>16-bit-sign-extend `(zcptr>aref ,array ,index)))

(defmacro zcptr>aset-s16b (value array index)
  `(zcptr>aset ,value ,array ,index))

(#-Symbolics defsetf #+Symbolics cl:defsetf zcptr>aref-s16b (array index) (value)
  `(zcptr>aset-s16b ,value ,array ,index))

(defsubst zcptr>null-p (ptr.ar ptr.idx)
  (and (null ptr.ar) (zerop ptr.idx)))

; This is so the code generator doesn't confuse itself.
(defmacro zcptr>flat-deref (ptr)
  ptr)

(defun zcptr>equal (arg1.ar arg1.idx arg2.ar arg2.idx)
  (and (eq arg1.ar arg2.ar)
	  (eql arg1.idx arg2.idx)))

(defun zcptr>compare-check (array-1 array-2)
  (and (neq array-1 array-2)
	  (or *compare-incomparable-pointers*
		 (ferror "Can't compare pointers to different arrays ~A and ~A"
				array-1 array-2))))

(defun zcptr>subtract-check (array-1 array-2)
  (or (eq array-1 array-2)
	 (ferror "Can't subtract pointers into different arrays, ~A and ~A"
			array-1 array-2)))


; ================================================================
; Compile a C source file.

; This is adapted from QC-FILE in SYS: QCFILE; (MIT system 98).
; We have different versions for LMITI and Symbolics 6.0 because they follow
; slightly different naming and pathname defaulting conventions, which I don't
; understand but have copied.  Note especially that this function is
; ZETA-C:CC-FILE in the LMIT version (to go with GLOBAL:QC-FILE) but
; ZETA-C:C-COMPILE-FILE in the Symbolics version (to go with
; COMPILER:COMPILE-FILE).  The TI version defines both names, with yet another
; interface for ZETA-C:C-COMPILE-FILE.
#-Symbolics   ; parameterized for MIT, LMI, TI
(defun cc-file (infile &optional outfile load-flag in-core-flag package-spec
				 file-local-declarations dont-set-default-p)
  "Compile C source file INFILE, producing a binary file and calling it OUTFILE.
PACKAGE-SPEC specifies which package to read the source in
 (usually the file's attribute list provides the right default).
LOAD-FLAG and IN-CORE-FLAG are semi-losing features; leave them NIL.
DONT-SET-DEFAULT-P says not to set the system default pathname to INFILE."
  #+TI (declare (values outfile status))
  ;; Default the specified input and output file names.  Open files.
  (setq infile (fs:merge-pathname-defaults infile fs:load-pathname-defaults nil))
  (with-open-stream (input-stream
				  (file-retry-new-pathname (infile fs:file-error)
				    (send infile ':open-canonical-default-type ':c)))
    ;; The input pathname might have been changed by the user in response to an
    ;;  error.  Also, find out what type field was actually found.
    (setq infile (funcall input-stream ':pathname))
    (or dont-set-default-p
	   (fs:set-default-pathname infile fs:load-pathname-defaults))
    (let ((generic-pathname (funcall infile ':generic-pathname))
		;; Must bind the following specials, though we don't use this feature.
		compiler:qc-file-macros-expanded
		(compiler:qc-file-record-macros-expanded t))
	 (setq outfile
		  (cond ((typep outfile 'fs:pathname)
			    (if (send outfile ':version) outfile
				 (send outfile ':new-pathname ':version
					  (funcall (funcall input-stream ':truename) ':version))))
			   (outfile
			    (fs:merge-pathname-defaults
				 outfile infile
				 (si:pathname-default-binary-file-type generic-pathname)
				 (funcall (funcall input-stream ':truename) ':version)))
			   (t
			    (funcall infile ':new-pathname
					   ':type (si:pathname-default-binary-file-type generic-pathname)
					   ':version (funcall (funcall input-stream ':truename)
									  ':version)))))
	 ;; Get the file plist again, in case we don't have it already or it changed
	 (fs:read-attribute-list generic-pathname input-stream)
	 ;; Bind all the variables required by the file property list.
	 (nlet ((vars vals (fs:file-attribute-bindings generic-pathname)))
	   (progv vars vals
		(unless (c-package-p *package*)
		  (ferror "Package ~A is not a C program package" *package*))
		(with-open-file (compiler:fasd-stream
					   outfile
					   :direction :output :characters nil
					   :byte-size 16. :if-exists :supersede)
		  (compiler:locking-resources
		    (setq outfile (funcall compiler:fasd-stream ':pathname))
		    (compiler:fasd-initialize)
		    (compiler:fasd-start-file)
		    (with-open-stream (parse-stream (make-c-parser input-stream infile))
			 (zcenv>clear-file-declarations (send parse-stream :source-file-symbol))
			 (compiler:compile-stream parse-stream generic-pathname
								 compiler:fasd-stream
								 'compiler:qc-file-work-compile
								 load-flag in-core-flag package-spec
								 file-local-declarations nil t))
		    (compiler:fasd-end-whack)
		    (compiler:fasd-end-file)))))))
  (values outfile #+TI compiler:*return-status*))

; This is for Genera 7.
#+Genera
(defun c-compile-file (infile &optional outfile in-package dont-set-default-p)
  "Compile C source file INFILE, producing a binary file and calling it OUTFILE.
IN-PACKAGE specifies which package to read the source in
 (usually the file's attribute list provides the right default).
DONT-SET-DEFAULT-P says not to set the system default pathname to INFILE."
  (declare (values binary source-truename binary-truename))
  (nlet ((pathname (fs:parse-pathname infile nil fs:load-pathname-defaults))
	    (in-package (and in-package (pkg-find-package in-package)))
	    ((fs:*package-root* (or in-package *c-package*))))
    (error-restart (error "Retry C-compiling ~A" infile)
	 (catch-error-restart (error "Skip C-compiling ~A" infile)
	   (sys:with-open-file-search (input-stream
							  ('c-compile-file fs:load-pathname-defaults t)
							  (c-compile-type-list pathname))
		(setq pathname (send input-stream :pathname))
		(unless dont-set-default-p
		  (fs:set-default-pathname pathname fs:load-pathname-defaults))
		(nlet ((generic-pathname (send pathname :generic-pathname))
			  ((outfile
				(if outfile
				    (fs:merge-pathnames outfile
								    (send pathname :new-canonical-type
										si:*default-binary-file-type*))
				  (send pathname :new-pathname :version :newest
					   :canonical-type si:*default-binary-file-type*)))))
		  (fs:read-attribute-list generic-pathname input-stream)
		  (let ((package
				(or in-package
				    (pkg-find-package (send generic-pathname :get :package)))))
		    (unless (c-package-p package)
			 (ferror "Package ~A is not a C program package" package))
		    (with-open-stream (parse-stream (make-c-parser input-stream infile))
			 (zcenv>clear-file-declarations
			   (send parse-stream :source-file-symbol))
			 (values
			   outfile
			   (send input-stream :truename)
			   (si:writing-bin-file (compiler:*binary-output-stream* outfile)
				(compiler:compile-from-stream
				  parse-stream generic-pathname 'compiler:compile-to-file
				  (and in-package (list ':package in-package)))))))))))))

; This is for Rel 6.
#+(and Symbolics (not Genera))
(defun c-compile-file (infile &optional outfile in-package dont-set-default-p)
  "Compile C source file INFILE, producing a binary file and calling it OUTFILE.
IN-PACKAGE specifies which package to read the source in
 (usually the file's attribute list provides the right default).
DONT-SET-DEFAULT-P says not to set the system default pathname to INFILE."
  (let ((pathname (fs:parse-pathname infile nil fs:load-pathname-defaults))
	   (in-package (and in-package (pkg-find-package in-package))))
    (sys:with-open-file-search (input-stream ('c-compile-file fs:load-pathname-defaults t)
									(c-compile-type-list pathname))
	 (setq infile (send input-stream :pathname))
	 (or dont-set-default-p (fs:set-default-pathname infile fs:load-pathname-defaults))
	 (nlet ((generic-pathname (send infile :generic-pathname))
		   ((outfile
			 (if outfile
				(fs:merge-pathnames outfile (send pathname :new-canonical-type
										    si:*default-binary-file-type*))
			   (send pathname :new-pathname :version :newest
				    ':canonical-type si:*default-binary-file-type*)))))
	   (fs:read-attribute-list generic-pathname input-stream)
	   (let ((fonts (send generic-pathname :get :fonts)))
		(when (and (listp fonts) (cdr fonts))
		  (setq input-stream
			   (zwei:make-encapsulated-font-decoding-stream input-stream))))
	   (let ((package (or in-package
					  (pkg-find-package (send generic-pathname :get :package)))))
		(unless (c-package-p package)
		  (ferror "Package ~A is not a C program package" package))
		(with-open-stream (parse-stream (make-c-parser input-stream infile))
		  (zcenv>clear-file-declarations (send parse-stream :source-file-symbol))
		  (lbin:writing-bin-file (compiler:*binary-output-stream* outfile)
		    (compiler:compile-from-stream
			 parse-stream generic-pathname 'compiler:compile-to-file
			 (and in-package (list ':package in-package))))))))))

#+Symbolics
(defun c-compile-type-list (pathname)
  (if (not (memq (send pathname :type) '(nil :unspecific)))
	 (values (list (send pathname :type)) pathname)
    (values '("C") pathname)))

; The TI version with their new interface (Rel 2).
#+TI
(defun c-compile-file (&optional input-file
				   &key output-file load set-default-pathname
				   (verbose compiler:compiler-verbose verbose-p)
				   declare ((:package pkg)))
  "Compiles C source file INPUT-FILE, writing the object to OUTPUT-FILE.  The
default for OUTPUT-FILE is <INPUT-FILE-name>.XFASL; the default for INPUT-FILE is
taken from FS:LOAD-PATHNAME-DEFAULTS.  Other keyword arguments:
  :LOAD, if true, means to load the object file after compiling.
  :VERBOSE, if true, means to print the name of each function being compiled.
  :SET-DEFAULT-PATHNAME, if true, means to set the default pathname for the next
    compile or load.
  :PACKAGE overrides the package specified in the file's attribute list.
  :DECLARE is a list of declaration specifiers.
Returns two values: 1) the output file pathname, and 2) a status code, one of:
0 (OK), 10 (warnings), 20 (errors), or 30 (fatal errors)."
  (declare (arglist &optional input-file &key output-file load verbose
				set-default-pathname package declare))
  (declare (values output-file error-status))
  (nlet ((compiler:compiler-verbose verbose)
	    ((outfile status
		 (cc-file (or input-file "") output-file nil nil pkg
				(if (or (null declare) (listp (first declare))) declare
				  (list declare))
				(not set-default-pathname)))))
    (when (and load (< status compiler:fatal))
	 (if verbose-p (load outfile :verbose verbose) (load outfile)))
    (values outfile status)))

; For use by MAKE-SYSTEM.
#-Symbolics
(defun cc-file-1 (infile outfile)
  #-TI (cc-file infile (send outfile :new-version nil) nil nil si:*force-package*)
  #+TI (nlet ((outfile error-code
			(lexpr-funcall #'c-compile-file infile
						:output-file (send outfile :new-version nil)
						:package si:*force-package* :verbose nil
						si:*mk-sys-compiler-options*)))
	    (unless si:*silent-p*
		 (format t " - #~D" (si:system-get-version-num outfile)))
	    (when (and (not si:*batch-mode-p*)
				( error-code compiler:errors)
				(not (y-or-n-p "Errors were reported during this compilation.  Continue anyway? ")))
		 (*throw 'exit-make-system nil)))
  (when si:*warnings-stream*
    (si:print-file-warnings infile si:*warnings-stream*)
    (send si:*warnings-stream* :send-if-handles :force-output)))


; ================================================================
; For debugging of all the many macros in this system.

(defun macrotest (form)
  (cond ((null form) (values nil nil))
	((nlistp form) (values form nil))
	((listp (car form))
	 (nlet ((new-car car-changed (macrotest (car form)))
		   (new-cdr cdr-changed (macrotest-1 (cdr form))))
	   (values (cons new-car new-cdr) (or car-changed cdr-changed))))
	((eq (car form) 'quote) form)
	((and (neq (car form) 'if)
	      (fboundp (car form)) (listp (fsymeval (car form)))
	      (eq (car (fsymeval (car form))) 'macro))
	 (values (funcall (cdr (fsymeval (car form))) form) t))
	((eq (car form) 'if)
	 (nlet ((new-pred pred-changed (macrotest (cadr form)))
		   (new-cons cons-changed (macrotest (caddr form)))
		   (new-alt alt-changed (and (cdddr form) (macrotest (cadddr form)))))
	   (values `(if ,new-pred ,new-cons . ,(and (cdddr form) `(,new-alt)))
			 (or pred-changed cons-changed alt-changed))))
;	((eq (car form) 'let)
;	 (multiple-value-bind (new-args any-changed-p)
;	     (macrotest-1 (cddr form))
;	   (values `(let ,(cadr form) . ,new-args) any-changed-p)))
	((eq (car form) 'do)
	 (multiple-value-bind (new-args any-changed-p)
	     (macrotest-1 (cdddr form))
	   (values `(do ,(cadr form) ,(caddr form) . ,new-args) any-changed-p)))
	(t (multiple-value-bind (new-args any-changed-p)
	       (macrotest-1 (cdr form))
	     (values (cons (car form) new-args) any-changed-p)))))

(defun macrotest-1 (forms)
  (if (null forms)
      (values nil nil)
    (multiple-value-bind (new-form changed-p)
	   (macrotest (car forms))
	 (if changed-p
		(values (cons new-form (cdr forms)) t)
	   (multiple-value-bind (new-forms any-changed-p)
		  (macrotest-1 (cdr forms))
		(values (cons (car forms) new-forms) any-changed-p))))))

(defun mt (form)
  `(mt ',(macrotest form)))

(defun mtall (form)
  (multiple-value-bind (res changed-p)
      (macrotest form)
    (if changed-p (mtall res) res)))


; End of ZCMISC.LISP
