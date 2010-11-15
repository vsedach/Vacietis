; This file contains miscellaneous macros and useful functions for Zeta-C.

(defmacro tail-push (item list)
  "/"Push/" an item onto the end of a list (rather than the beginning.)"
  `(setf ,list (nconc ,list (ncons ,item))))

(defun cons-if-non-nil (obj list)
  "If OBJ is non-nil, conses it onto LIST; otherwise just returns LIST."
  (if obj (cons obj list) list))

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

(defsubst neql (a b)
  (not (eql a b)))

(defun eqv (a b)
  "Logical equivalence predicate, in terms of non-NIL//NIL rather than bitwise."
  (or (and a b) (and (not a) (not b))))

(defun upto-next (number increment)
  "Returns the smallest number greater than or equal to NUMBER which is an integer
   multiple of INCREMENT."
  (* increment (ceiling number increment)))

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
	     (progn ,@(cdr (assq ':abort clauses)))
             ,@(cdr (assq ':normal clauses)))))))

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
  (if *expanding-defunc+*
	 (lexpr-funcall #'ferror (string-append "Error while C-compiling ~A:~%" fmt)
				 (cons *expanding-defunc+* args))
    (lexpr-funcall #'ferror fmt args)))

(defun zcwarn (fmt &rest args)
  (let ((func *expanding-defunc+*))
    (if func
	   (lexpr-funcall #'compiler:warn `(:function ,func) #-Symbolics nil fmt args)
	 (send error-output ':fresh-line)
	 (lexpr-funcall #'format error-output fmt args))))

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
