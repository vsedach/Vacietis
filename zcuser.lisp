; This file contains the C listener and debugging support code.


; ================================================================
; The C listener.

(defvar *c-listener-prompt* "C: "
  "The prompt printed by the top level C listener.")

(defvar *debug-code-gen-p* nil
  "Causes the C listener to print translated forms before evaluating them.")

(defvar *last-form* nil
  "The translation of the last form entered to the C listener.")

(defflavor c-listener
	   ((c-listener-package nil))
	   (tv:process-mixin #-Symbolics tv:full-screen-hack-mixin
	    #-Genera tv:window #+Genera dw:dynamic-window)
  (:default-init-plist
   :save-bits t
   :process '(c-top-level :regular-pdl-size 4096.)
   #+Genera :margin-components
   #+Genera '((dw:margin-borders :thickness 1)
	      (dw:margin-white-borders :thickness 3)
	      (dw:margin-scroll-bar)
	      (dw:margin-label :margin :bottom
			       :style (:sans-serif :italic :normal))
	      (dw:margin-scroll-bar :margin :bottom)
	      (dw:margin-whitespace :margin :left :thickness 3)
	      (dw:margin-whitespace :margin :bottom :thickness 3)))
  (:documentation "C Listener window"))

(defmethod (c-listener :package) ()
  c-listener-package)

(defmethod (c-listener :set-package) (pkg)
  (setq c-listener-package (pkg-find-package pkg)))

(compile-flavor-methods c-listener)

(#+3600 tv:add-select-key #-3600 tv:add-system-key #/{ 'c-listener "C Listener")

#+Symbolics
(progn
  (tv:add-to-system-menu-programs-column
    "C" '(tv:select-or-create-window-of-flavor 'c-listener)
    "Select a C listener, to read and evaluate C expressions interactively."
    "Mail")
  (tv:add-to-system-menu-create-menu
    "C" 'c-listener
    "A C listener, in which to read and evaluate C forms interactively."
    "Inspect"))

#-Symbolics
(progn
  (unless (string-equal (caar tv:*system-menu-programs-column*)
			#+TI "C Listener    " #+MIT "C Listener")
    (push '(#+TI "C Listener    " #+MIT "C Listener"
	    :eval (tv:select-or-create-window-of-flavor 'c-listener)
	    :documentation
	    #+TI (:mouse-any "Select a C listener, to read and evaluate C expressions interactively.")
	    #+MIT "Select a C listener, to read and evaluate C expressions interactively.")
	  tv:*system-menu-programs-column*)
    (push '("C" :value c-listener
	    :documentation "A C listener, in which to read and evaluate C forms interactively.")
	  (cdr (mem #'(lambda (insert-point item)
			(string-equal (car item) insert-point))
		    "Telnet" tv:default-window-types-item-list)))
    (clear-resource 'tv:window-type-menu)))

(defun c-top-level (terminal-io &optional break-p default-pathname)
  "Top level C listener read//eval//print loop."
  (nlet ((package (zclstn>verify-c-package terminal-io break-p))
	 ((_sym (intern "_"))
	  (__sym (intern "__"))
	  (___sym (intern "___"))
	  (*last-form* nil))
	 (base 10.) (ibase 10.)
	 #-Genera (*nopoint nil) #+Genera (cl:*print-radix* t)
	 (standard-input standard-input)
	 (standard-output standard-output)
	 (error-output error-output)
	 (where-we-are (if break-p
			   (format nil "Breakpoint.  Type ~C to continue; ~C to quit."
				   #\Resume #\Abort)
			 (format nil "C top level in ~A." (send terminal-io :name)))))
    (zclstn>setup-_-vars _sym __sym ___sym)
    (if (and (not break-p)
	       (y-or-n-p (format nil "Initialize externals and statics in package ~A? "
				 package)))
	(zclib>initialize-program package)
      (send standard-output :fresh-line))
    (zclib>with-c-io (or default-pathname (zclstn>request-default-pathname))
      (with-open-stream (c-stream (make-c-parser terminal-io *default-pathname*))
	(format t "~&//* ~A *//" where-we-are)
	(format t "~&//* Reading C in package ~A. *//" package)
	(unless break-p
	  (format t "~&//* Type Meta-~C to change packages. *//"
		  #\Abort))
	;; *CATCH works in all systems, and we don't care about the return values.
	(*catch 'return-from-c
	  (loop
	    (nlet ((c-form (prog1 (zclstn>read c-stream break-p)
				  (send standard-output :fresh-line)))
		   ((ignore error-p
			    (catch-error-restart ((sys:abort error) "Return to ~A"
						  (if break-p "Breakpoint."
						    where-we-are))
			      (nlet ((value type (zclstn>eval c-form)))
				(send terminal-io :fresh-line)
				(when (and (listp c-form)
					   (or (eq (car c-form) 'c:defunc+)
					       (and (eq (car c-form) 'si:displaced)
						    (eq (caadr c-form) 'c:defunc+)))
					   (or (not *debug-code-gen-p*)
					       (y-or-n-p (format nil "Compile ~A? "
								 value))))
				  (compile value))
				(unless (or (and (null value)
						 (not (zctype>lispval-p type)))
					    (eq type ':toplevel)
					    (and (listp value)
						 (eq (car value)
						     ':|No value returned from|)))
				  (zclstn>shift-types-and-values _sym __sym ___sym
								 value type)
				  (zclstn>print value type terminal-io)))))))
	      (when error-p
		(send c-stream :close-includes)
		(send c-stream :reset-interactive-input)
		(format t "~&//* Back to ~A *//" where-we-are)))))
	(values)))))

(defun zclstn>verify-c-package (win just-use-default-if-ok)
  "Reads a package name from the user, verifying that it is suitable for reading
   C in.  If JUST-USE-DEFAULT-IF-OK, the default package is used if it is suitable."
  (do ((tpkg (and just-use-default-if-ok
		  (or (send win :send-if-handles :package) package))))
      ((and tpkg (c-package-p tpkg))
       (send win :send-if-handles :set-package tpkg)
       tpkg)
    (nlet ((default (or (send win :send-if-handles :package)
			(and (c-package-p package) package)
			*c-user-package*))
	   ((name (string-upcase
		    (prompt-and-read :string
				     "~&C package to work in~@[ (default ~A)~]? "
				     default)))
	    ((name (if (string-equal name "") default name))
	     ((pkg (pkg-find-package name :find))))))
      (cond ((null pkg)
	     (and (y-or-n-p (format nil "Create C package ~A? " name))
		  (setq tpkg (create-c-package name))))
	    ((not (c-package-p name))
	     (format t "~A is not a C package (one which inherits directly from /"C:/").~%"
		     name))
	    (t (setq tpkg pkg))))))

(defun zclstn>setup-_-vars (&rest vars)
  (mapc #'(lambda (sym)
	    (unless (boundp sym)
	      (set sym 0)
	      (zcenv>declare-type sym (zctype>int) (zcenv>global-env))
	      ;; Strictly, only needed for LMIT 102 and later, but why not.
	      (putprop sym t 'special)
	      (nlet ((array-sym index-sym (zcprim>pointer-var-pair sym)))
		(putprop array-sym t 'special)
		(putprop index-sym t 'special))))
	vars))

(defun zclstn>request-default-pathname ()
  (nlet ((default (zclib>extract-directory (fs:default-pathname)))
	 ((wdir
	    #+Symbolics
	    (prompt-and-read `(:pathname :visible-default ,default)
			     "~&Working directory? ")
	    #-Symbolics
	    (prompt-and-read `(:pathname :default ,default)
			     "~&Working directory? (default ~A) " default))
	  ((wdir (zclib>extract-directory wdir)))))
    (format terminal-io "~&//* Working directory is ~A. *//" wdir)
    (fs:set-default-pathname wdir)
    wdir))

; Sigh.  Divergence...
#+Symbolics
(defmacro with-input-editing-portably ((stream options) &body body)
  "Give me a break."
  `(with-input-editing-options ,options
     (with-input-editing (,stream)
       . ,body)))

#-Symbolics
(defmacro with-input-editing-portably ((stream options) &body body)
  "Give me a break."
  `(with-input-editing (,stream ',options) . ,body))

#-Genera
(push '(with-input-editing-portably 0 3 1 1) zwei:*lisp-indent-offset-alist*)
#+Genera
(puthash 'with-input-editing-portably '(0 3 1 1)
	 zwei:*lisp-indentation-offset-hash-table*)


#-Symbolics 
(eval-when (compile)
  (format t "~&ZCLSTN>READ changed again -- be sure to test new version here!~@
	     Type any character to continue: ")
  (tyi))
(defun zclstn>read (c-stream break-p)
  "Hairy read function for the C listener -- handles various cases of aborting,
   prompting, etc."
  (if (not (send c-stream :interactive))     ; If user has done a #include,
      (send c-stream :read)		     ;  we just read the next form.
    (let ((full-rubout-prev nil))
      (do-named form-read () (())
	(*catch 'retry
	  (unless full-rubout-prev (format terminal-io "~&~A" *c-listener-prompt*))
	  (setq full-rubout-prev nil)
	  (let ((firstchar
		  (let ((tv:kbd-intercepted-characters
			  #+Symbolics '(#\m-Suspend)
			  #-Symbolics
			  `(,(assq #\m-Break tv:kbd-intercepted-characters))))
		    (send terminal-io :tyi))))
	    (cond ((and break-p (char= firstchar #\Resume))
		   (format terminal-io "[Resume]~%")
		   (*throw 'return-from-c nil))
		  ((char= firstchar #\Break)
		   (catch-error-restart ((sys:abort error) "Return to C top level.")
		     (break "C-LISTENER"))
		   (*throw 'retry nil))
		  ((char= firstchar #\Abort)
		   (format terminal-io "[Abort]~%")
		   (if break-p (signal 'sys:abort #-Symbolics "")
		     (*throw 'retry nil)))
		  ((char= firstchar #\m-Abort)
		   (format terminal-io "[Abort all]~%")
		   (send current-process :reset :always))
		  ((char= firstchar #\Rubout)
		   (setq full-rubout-prev t)
		   (*throw 'retry nil))
		  (t (send terminal-io :untyi firstchar))))
	  (nlet ((ignore error-p
		   (catch-error-restart ((sys:abort error) "Return to C top level.")
		     (with-input-editing-portably
		         (terminal-io
			   ;; MIT 99 won't evaluate *c-listener-prompt*.
			   ((:reprompt #+MIT "C: " #-MIT *c-listener-prompt*)
			    ;; I hate trivial incompatibilities...
			    (:activation #+Symbolics #'eql #-Symbolics eql #\End)
			    (:full-rubout :full-rubout)))
		       (nlet ((form full-rubout-p (send c-stream :read)))
			 (if (neq full-rubout-p :full-rubout)
			     (return-from form-read form)
			   (setq full-rubout-prev t)
			   (*throw 'retry nil)))))))
	    (when error-p
		(send c-stream :close-includes)
		(send c-stream :reset-interactive-input)
		(format t "~&//* Back to C top level in ~A. *//"
			(send terminal-io :name)))))))))

#-Symbolics
(defmacro compiler:compiler-warnings-context-bind-maybe (&body body)
  `(progn . ,body))

(defun zclstn>eval (c-form)
  "Evaluates a top-level C form.  This is used by the C listener.  Returns two
   values: the value of the form, and its C type."
  (compiler:compiler-warnings-context-bind-maybe
    (nlet ((trans-form type
	    (if (and (listp c-form) (memq (car c-form)
					  '(c:decl+ c:defunc+ zcenv>#define)))
		(values c-form ':toplevel)
	      (zcprim>translate-for-top-level c-form (zcenv>global-env)))))
      (selectq *debug-code-gen-p*
	(nil)
	(:grind (grind-top-level (if (eq type ':toplevel)
				     (setq trans-form (macroexpand trans-form))
				   trans-form)
				 nil terminal-io))
	(t (format terminal-io "~&=> ~S~%"
		   (if (eq type ':toplevel)
		       (setq trans-form (macroexpand trans-form))
		     trans-form))))
      (setq *last-form* trans-form)
      (if type
	  (values (eval trans-form) type)
	(eval `(prog () . ,trans-form))))))

(defun zclstn>shift-types-and-values (_sym __sym ___sym value type)
  "Shifts the values and types of previously evaluated expressions down the
   chain of _-variables."
  (nlet ((genv (zcenv>global-env))
	 ((_type (or (zcenv>type _sym genv) (zctype>int)))   ; Don't know how these
	  (__type (or (zcenv>type __sym genv) (zctype>int)))))       ; become NIL.
    (set ___sym nil)
    (zcprim>declare-external ___sym __type)
    (eval (zcprim>store-value ___sym __type __sym __type genv nil))
    (set __sym nil)
    (zcprim>declare-external __sym _type)
    (eval (zcprim>store-value __sym _type _sym _type genv nil))
    (set _sym nil)
    (zcprim>declare-external _sym type)
    (eval (zcprim>store-value _sym type
			      (zcprim>translate-for-top-level `(c:quote+ ,value ,type)
							      (zcenv>global-env))
			      type genv nil))))

(defun zclstn>print (value type stream)
  "Prints a value for the C listener."
  (send stream :tyo #/()
  (zclstn>print-type type (zcenv>global-env) stream)
  (format stream ") ")
  (zclstn>print-value value type stream))

(defun zclstn>type-string (type &optional (env (zcenv>global-env)) (short-p :short))
  "Returns a string representation of TYPE in ENV.  See ZCLSTN>PRINT-TYPE re SHORT-P."
  (with-output-to-string (s) (zclstn>print-type type env s short-p)))

; Dammit, this still isn't right.  Must rethink the whole idea.  How can we tell
; when to insert parentheses?
(defun zclstn>print-type (type env stream &optional short-p (insert ""))
  "Prints a representation of TYPE (in ENV) on STREAM.  SHORT-P controls the printing
   of structure types; if NIL, the full element list is printed; if :USE-TAG, the tag
   is printed if there is one, else the full element list; if :SHORT, the tag is
   printed if any, else /"{...}/"."
  (cond ((or (zctype>number-p type) (zctype>void-p type) (zctype>lispval-p type))
	 (format stream "~(~A~)~A" type insert))
	((zctype>bits-p type)
	 (zclstn>print-type (cadr type) env stream short-p
			    (format nil "~A:~D" insert (caddr type))))
	((zctype>function-p type)
	 (zclstn>print-type (zctype>function-return-type type) env stream short-p
			    (string-append insert "()")))
	((zctype>array-p type)
	 (zclstn>print-type (zctype>pointer-deref-type type) env stream short-p
			    (format nil "~A[~@[~D~]]"
				    insert (zctype>array-length type))))
	((zctype>pointer-p type)
	 (zclstn>print-type (zctype>pointer-deref-type type) env stream :short
			    (string-append " *" insert)))
	((zctype>struct-p type)
	 (format stream "~A "
		 (cdr (assq (zctype>struct-class type)
			    '((:struct . "struct") (:packed-struct . "packed_struct")
			      (:union . "union")))))
	 (cond ((and short-p (zctype>struct-tag type))
		(format stream "~A~A" (zctype>struct-tag type)
			insert))
	       ((and short-p (neq short-p :use-tag))
		(format stream "{...}~A" insert))
	       (t
		(format stream "{ ")
		(mapc #'(lambda (pair)
			  (when (neq (zctype>eltpr-name pair) ':end)
			    (zclstn>print-type (zctype>eltpr-type pair) env stream
					       (or short-p :use-tag)
					       (format nil " ~A"
						       (zctype>eltpr-name pair)))
			    (format stream "; ")))
		      (zctype>struct-elements type env))
		(format stream "}~A" insert))))
	(t (ferror "Don't know how to print type ~A" type))))

(defvar *debug-printer-p* nil
  "Turn this on if you want to handle printer errors with the debugger.")

(defun zclstn>print-value (value type stream &optional recursive)
  (if *debug-printer-p*
      (zclstn>print-value-1 value type stream recursive)
    (nlet ((ignore error-p
	     (catch-error (zclstn>print-value-1 value type stream recursive) nil)))
;	     (zclstn>print-value-1 value type stream recursive)))
      (and error-p (format stream "<< Error printing value; trying Lisp printer: >> ~S"
			   value)))))
(defun zclstn>print-value-1 (value type stream recursive)
  (cond ((zctype>void-p type) (format stream "#{VOID}"))
	((zctype>lispval-p type) (format stream "~S" value))
	((zctype>char-p type)
	 (format stream "'~C'" value))
	((zctype>integer-p type)
	 (zclstn>print-integer value type stream recursive))
	((zctype>float-p type)
	 (format stream "~F" value))
	((zctype>array-p type)
	 (if (zctype>char-p (zctype>pointer-deref-type type))
	     (zclstn>print-string value stream 0
				  (min 60 (+ (if recursive 10 30)
					     (zctype>array-length type))))
	   (if (and recursive (neq recursive ':arrays))
	       (if (zcptr>ptr-p value)
		   (zclstn>print-element value type stream)
		 (zclstn>print-short-rep value stream))
	     (zclstn>print-array-explicit value type stream))))
	((zctype>function-pointer-p type)
	 (format stream "~A" value))
	((and (zctype>pointer-p type)
	      (zcptr>null-p (zcptr>array value) 0))
	 (format stream "NULL~:[ + ~D~]"
		 (zerop (zcptr>index value)) (zcptr>index value)))
	((zctype>pointer-p type)
	 (cond ((and recursive
		     (zclstn>fancy-print-char-pointer value type stream recursive)))
	       ((eq (zcptr>array value) ':stdio)
		;; Special hack for stdio streams.
		(format stream "Pointer to ~A, fd = ~D"
			(zclib>fd-to-stream (zcptr>index value) nil nil)
			(zcptr>index value)))
	       ((or (not (arrayp (zcptr>array value)))
		    (not (fixp (zcptr>index value))))
		(format stream "Invalid pointer with array part ~S, index part ~S"
			(zcptr>array value) (zcptr>index value)))
	       ((eq 'value-cell (named-structure-p (zcptr>array value)))
		(format stream "Pointer to ~A's value cell~:[ + ~D~]"
			(car (zcprim>array-desc (zcptr>array value)))
			(zerop (zcptr>index value))
			(// (zcptr>index value)
			    (zctype>sizeof-in-scale (zctype>pointer-deref-type type)
						    (zcenv>global-env)))))
	       (t (zclstn>print-element value type stream)))
	 (unless recursive (zclstn>fancy-print-char-pointer value type stream)))
	((zctype>struct-p type)
	 (zclstn>print-struct-explicit value type stream))
	(t (ferror "Don't know how to print value ~A of type ~A" value type))))

(defun zclstn>print-integer (value type stream recursive)
  (nlet ((infinity-relative (and (zctype>unsigned-p type) (minusp value)))
	 (len (haulong value))
	 ((octlen (max #+3600 30 #-3600 24 (+ 3 (upto-next len 3))))
	  (hexlen (max #+3600 32 #-3600 24 (+ 4 (upto-next len 4))))))
    (format stream "~:[~;~]~D" infinity-relative value)
    (when (and (not recursive) (> (abs value) 8))
      (format stream " = 0~:[~;...777...~]~O = 0x~:[~;...FFF...~]~16R"
	      infinity-relative (logand value (1- (ash 1 octlen)))
	      infinity-relative (logand value (1- (ash 1 hexlen)))))))

(defun zclstn>print-string (str stream start end &optional mark-index)
  ;; Arrays in flat mode can still be represented as consed pointers!
  (nlet ((str start end (if (arrayp str) (values str start end)
			  (values (zcptr>array str) (+ start (zcptr>index str))
				  (+ end (zcptr>index str))))))
    (send stream :tyo #/")
    (do ((i start (1+ i)))
	((or ( i end) ( i (array-length str))
	     (and (> i start) (= (aref-byte-safe str (1- i)) 0)
		  mark-index (< mark-index i)))
	 (when (and (< i (array-length str))
		    (not (= (aref-byte-safe str i) 0)))
	   (send stream :display-lozenged-string "...")))
      (when (and mark-index (= mark-index i))
	(send stream :display-lozenged-string "HERE->"))
      (when (< i end) (send stream :tyo (code-char (aref-byte-safe str i)))))
    (send stream :tyo #/")))

#+3600
(defun aref-byte-safe (str i)
  "Returns 0 if (aref str i) would get an /"array-word not fixnum/" error."
  (nlet ((scale-factor (// (zctype>scale-size ':Q)
			   (zctype>scale-size (zcprim>array-scale str))))
	 ((scaled-index (// i scale-factor))))
    (if (or ( scaled-index (// (array-length str) scale-factor))
	    (fixp (aref (zcprim>array-as-q str) scaled-index)))
	(aref str i)
      0)))

#-3600
(defun aref-byte-safe (str i)
  (aref str i))

(defun zclstn>fancy-print-char-pointer (ptr type stream &optional short-p)
  "Pretty way to print a character pointer.  Can be called with any pointer; will
   do nothing if it's not either a char pointer or a pointer to storage that was
   originally declared char."
  (nlet ((array (zcptr>array ptr))
	 (index (zcptr>index ptr))
	 (dtype (zctype>pointer-deref-type type))
	 (width (if short-p 10. 30.)))
    (unless (or (not (arrayp array)) (eq (named-structure-p array) 'extern-dummy))
      (zcprim>check-array-leader array)
      (nlet ((actual (if (neq (named-structure-p array) 'cast-array) array
		       (zcprim>array-desc array)))
	     (index (* index (zctype>scale-size (zctype>type-scale dtype))))
	     ((desc (zcprim>array-desc actual))
	      ((desc (if (listp desc) desc
		       `(nil ,(zctype>array-of
				(zctype>char)
				(if (numberp desc) desc (array-length actual))))))
	       ((array (zcprim>array-as-8b actual)))
	       ((elt-type start len
		  (zclstn>element-type index :8B (cadr desc) (caddr desc)))))))
	(and (or (zctype>char-p dtype) (zctype>char-p elt-type))
	     ( index 0) (< index (array-length array))
	     (nlet ((elt-size (zctype>scale-size (zctype>type-scale elt-type)))
		    ((start (max (* start elt-size) (- index width))))
		    ((len (* len elt-size))))
	       (progn (unless short-p (format stream "~&         i.e., "))
		      (zclstn>print-string array stream start
					   (min (+ index width) (+ start len))
					   index))))))))

(defun zclstn>element-type (index scale type env)
  "Given an index into an aggregate of type TYPE, returns the type of the scalar
   element which resides at that index.  Second value is the base of the array
   immediately containing that element, in the scale appropriate thereto, or INDEX 
   (again, rescaled) if there is no such array.  Third value is the size of that
   array (again, rescaled).  Returns NIL if the top-level aggregate is a struct, and
   the index is out of range for it."
  (nlet ((scaled-index (* index (zctype>scale-size scale)))
	 ((etype array-base array-size
	   (zclstn>element-type-1 scaled-index type env 0 0 nil t))))
    (and etype
	 (nlet ((esize (zctype>scale-size (zctype>type-scale etype))))
	   (values etype (// array-base esize)
		   (if (null array-size) 1 (// array-size esize)))))))
(defun zclstn>element-type-1 (index type env elt-base array-offset array-size top-level)
  (cond ((zctype>array-p type)
	 (nlet ((elt-type (zctype>pointer-deref-type type))
		((elt-len (zctype>sizeof elt-type env))
		 ((offset (\ index elt-len))))
		(array-size (* (or array-size 1) (zctype>sizeof type env))))
	   (zclstn>element-type-1 offset elt-type env (+ elt-base (- index offset))
				  (+ array-offset (- index offset))
				  array-size (and top-level ( index array-size)))))
	((zctype>struct-p type)
	 ;; Unions will require knowing the last tag stored.  Later...
	 (nlet ((found elt-offset elt-type extended
		 (and ( index 0)
		      (zctype>struct-offset-elt index :8B type env top-level))))
	   (and found (zclstn>element-type-1 (- index elt-offset) elt-type env
					     (+ elt-base elt-offset) 0 nil extended))))
	(t (values type (- elt-base array-offset)
		   (if top-level (+ array-size 99) array-size)))))

(defun zclstn>print-array-explicit (value type stream)
  "Prints a brace- and comma-delimited list of the elements of an array.  Works if
   given either a (Lisp) array or a pointer to a subarray of a structure; but the
   length given in the type must be correct."
  (if (eq (named-structure-p value) 'extern-dummy)   ; An exception.
      (zclstn>print-short-rep value stream)
    (nlet ((array (if (arrayp value) value (zcptr>array value)))
	   (length (zctype>array-length type))
	   ((desc (zcprim>array-desc array))
	    ((desc (if (arrayp desc) (zcprim>array-desc desc) desc))
	     ((env (caddr desc))))))
      (format stream "{ ")
      (do ((i 0 (1+ i))
	   (lim (min (ceiling 20 (zctype>sizeof (zctype>pointer-deref-type type) env))
		     length)))
	  (( i lim) (when (< i length) (format stream "...")))
	(nlet ((exp type (zcprim>translate-for-top-level
			   `(c:[] (c:quote+ ,value ,type) ,i) env)))
	  (if (and (zctype>array-p type)
		   (zctype>char-p (zctype>pointer-deref-type type)))
	      (zclstn>print-string (eval exp) stream 0
				   (min 60 (zctype>array-length type)))
	    ;; Too inflexible.  How do I print thee?  Let me count the ways...
	    (zclstn>print-value (eval exp) type stream :arrays)))
	(unless (= i (1- length))
	  (format stream ", ")))
      (format stream " }"))))

(defun zclstn>print-struct-explicit (value type stream)
  "Prints a brace- and comma-delimited list of the elements of a structure.  Works if
   given either a (Lisp) array or a pointer to a substructure of some other aggregate."
  (nlet ((array (if (arrayp value) value (zcptr>array value)))
	 ((env (zcprim>array-consed-env array))
	  ((elts (zctype>struct-elements type env)))))
    (format stream "{ ")
    (mapc #'(lambda (pair)
	      (unless (eq (zctype>eltpr-name pair) ':end)
		(nlet ((exp type (zcprim>translate-for-top-level
				   `(c:/. (c:quote+ ,value ,type) ,(car pair)) env)))
		  ;; Too inflexible.  How do I print thee?  Let me count the ways...
		  (zclstn>print-value (eval exp) type stream :arrays))
		(format stream "; ")))
	  elts)
    (format stream "}")))

(defun zclstn>print-element (pointer type stream)
  "Prints an accessor-expression representation of POINTER on STREAM."
  (zcprim>check-array-leader (zcptr>array pointer))
  (nlet ((array (zcptr>array pointer))
	 (index (zcptr>index pointer))
	 ((desc (zcprim>array-desc array))
	  (scale-size (zctype>scale-size (zcprim>array-scale array))))
	 (dtype (zctype>pointer-deref-type type)))
    (when (zctype>canonicalized-pointer-p type) (format stream "Pointer to "))
    (if (eq (named-structure-p array) 'cast-array)
	(let ((desc (zcprim>array-desc desc)))
	  (zclstn>print-short-rep array stream)
	  (nlet ((offset
		  (zclstn>print-element-1 (* index scale-size)
					  (cadr desc) (caddr desc)
					  (if (zctype>array-p type) type dtype)
					  stream t)))
	    (cond ((and (null offset) (zctype>struct-p (cadr desc)) (not (zerop index)))
		   (format stream " + ~D"
			   (// index (zctype>sizeof-in-qs dtype (caddr desc)))))
		  ((not (zerop offset))
		   (format stream ", ~A offset ~D"
			   (cdr (assq (zctype>type-scale dtype)
				      '((:Q . "word") (:16B . "halfword")
					(:8B . "byte"))))
			   (// offset
			       (zctype>scale-size (zcprim>array-scale array))))))))
      (zclstn>print-short-rep array stream)
      (nlet ((offset (zclstn>print-element-1
		       (* index scale-size) (cadr desc) (caddr desc)
		       (if (zctype>array-p type) type dtype)
		       stream t)))
	(when (and (null offset) (zctype>struct-p (cadr desc)) (not (zerop index)))
	  (format stream " + ~D"
		  (// index (zctype>sizeof-in-qs dtype (caddr desc)))))))))
(defun zclstn>print-element-1 (index type env orig-type stream top-level)
  (cond ((zctype>match type orig-type env)
	 (and ( index 0) (< index (zctype>sizeof type env)) index))
	((zctype>array-p type)
	 (nlet ((elt-type (zctype>pointer-deref-type type))
		((elt-len (zctype>sizeof elt-type env))))
	   (format stream "[~D]" (// index elt-len))
	   (zclstn>print-element-1 (\ index elt-len) elt-type env orig-type
				   stream nil)))
	((zctype>struct-p type)
	 ;; Unions will require knowing the last tag stored.  Later...
	 (nlet ((elt elt-offset elt-type
		 (and ( index 0)
		      (zctype>struct-offset-elt index :8B type env top-level))))
	   (if (null elt) nil
	     (format stream ".~A" elt)
	     (zclstn>print-element-1 (- index elt-offset) elt-type env orig-type
				     stream nil))))
	(t index)))

(defun zclstn>print-short-rep (object stream &optional already-started)
  "Prints a #{...} representation of OBJECT on STREAM."
  (zcprim>check-array-leader object)
  (let ((desc (zcprim>array-desc object)))
    (selectq (named-structure-p object)
      (cast-array
       (progn (format stream "#{~A view of " (array-type object))
	      (zclstn>print-short-rep desc stream t)))
      (extern-dummy
       (format stream "#{(")
       (zclstn>print-type (cadr desc) (caddr desc) stream :short)
       (format stream ") ~A, which has been declared /"extern/" but not initialized}"
	       (car desc)))
      (otherwise
       (let ((name (car desc))
	     (type (cadr desc))
	     (env (caddr desc)))
	 (unless already-started (format stream "#{"))
	 (tyo #/( stream)
	 (zclstn>print-type type env stream :short)
	 (cond ((eq (named-structure-p object) 'value-cell)
		(format stream ") ~A's value cell}" name))
	       (name (format stream ") ~A}" name))
	       (t (format stream ") ~O}" (%make-pointer dtp-fix object)))))))))

(defun zclstn>print-aggregate (ignore object stream ignore slashify)
  "General aggregate printer.  Appropriate for invocation from a NAMED-STRUCTURE-INVOKE
   property."
  (ignore slashify)                          ; For now.  What would this do?
  (zclstn>print-short-rep object stream))

;; This is just a quick hack -- better version later.
(defun zclstn>describe-aggregate (ignore object)
  (nlet ((desc (zcprim>array-desc object))
	 ((desc (if (arrayp desc) (zcprim>array-desc desc) desc))))
    (send standard-output :fresh-line)
    (zclstn>print-value object (cadr desc) standard-output)))

(defselect zclstn>aggregate-invoke
  (:print-self . zclstn>print-aggregate)
  (:describe . zclstn>describe-aggregate))

(deff (:property array named-structure-invoke) 'zclstn>aggregate-invoke)
(deff (:property struct named-structure-invoke) 'zclstn>aggregate-invoke)
(deff (:property union named-structure-invoke) 'zclstn>aggregate-invoke)
(deff (:property value-cell named-structure-invoke) 'zclstn>aggregate-invoke)
(deff (:property cast-array named-structure-invoke) 'zclstn>aggregate-invoke)
(deff (:property extern-dummy named-structure-invoke) 'zclstn>aggregate-invoke)

; Don't turn this on yet, breaks inspector
; Inspector interface.  This simple hack gives us control.
;(defwhopper (tv:basic-inspect :object-named-structure) (obj)
;  (let ((nss (named-structure-p obj)))
;    (if (or (eq (symbol-package nss)
;               (pkg-find-package 'zeta-c :error si:pkg-global-package))
;           (memq (pkg-find-package 'c :error si:pkg-global-package)
;                 (package-use-list (symbol-package nss))))
;       (zclstn>inspect-aggregate obj)
;     (continue-whopper obj))))



; End of ZCUSER.LISP
