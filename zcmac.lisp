; This file contains the macro-expansion system for Zeta-C, which drives the
; C-to-Lisp translation, and the primitive-defining macro.

; ================================================================
; The guts of the primitive-definition and form-translation system.

; OVERVIEW: The various C operators and primitives -- +, if, etc. -- are macros
; in the C package.  Since some of them can take varying numbers of arguments,
; and expand completely differently in the various cases -- * for instance -- we
; provide a facility, DEFPRIM, to assist in the writing of these macros.
;
; A DEFPRIM looks like this:
; (defprim c:#
;   (((arg1 type1 :boolean))
;    ... body for unary case ...)
;   (((arg1 type1 :binary) (arg2 type2 :binary))
;    ... body for binary case ...))
;
; This form defines a macro, c:#, that has both unary and binary uses.  If it's
; called with one argument, ARG1 will be bound to the translated argument
; expression, TYPE1 will be bound to its type, and then the body for the unary
; case will be executed (we'll come back to :BOOLEAN in a moment).  That body
; is expected to return two values: the translated Lisp for the # form, and the
; data type (:INT etc.) of the # expression.  Similarly for the binary case.
;
; DEFPRIM argument patterns also support &quote/&eval, &rest, and &body.  &quote
; means succeeding arguments should not be translated, up to the next &eval, if
; any.  A quoted argument in a pattern must be a symbol, not a list; for instance:
; (defprim foo
;   (((arg1 type1) &quote arg2)))
; The difference between &rest and &body (unless preceded by &quote, in which case
; they are the same) is that an &rest parameter will bind both the translated
; arguments and their types, while an &body will just bind the arguments.  E.g.:
;   ((arg1 type1) &rest (args types))
; but
;   ((arg1 type1) &body (body :statement))   ; see below re :STATEMENT
; See ZCPRIM for examples.
;
; The expansion of an inner form depends on certain information about the context
; in which it appears.  In ZETA-C, this information is provided in two forms.
; One of these is the "environment", that gives information about the types of
; various identifiers.  DEFPRIM bodies can access the environment in which they
; appear through the pseudo-special variable **ENV (see ZCTYPE for its structure).
; Arguments to a form are normally expanded in the same environment as the form.
; In contrast, the pseudo-special variable **CONTEXT is a list with an entry for
; every construct in which the current one is nested, in inside-out order.  So,
; for instance, in the unary case of the # primitive defined above, its argument
; will be expanded with (:BOOLEAN) consed onto **CONTEXT.[[This is done because
; Lisp uses NIL and non-NIL as boolean values, while C uses zero and nonzero.
; If a value is to be used by a conditional, we want it to be NIL or non-NIL,
; while if it is going to be stored in a variable or used in some arithmetic
; operation, we want it to be 0 or 1.]]
; Here are the various symbols used in **CONTEXT, and what they indicate:
;   :BOOLEAN		The value will be used as a boolean.
;   :LVALUE		The value will be used as an lvalue.
;   :STATEMENT		The value will be thrown away (i.e. is being computed
;				for effect).
;   :UNARY		The value will have the standard unary coercions applied to it.
;   :BINARY		(Must be used in pairs) The value, along with the other
;				argument value marked as :BINARY, will have the standard
;				binary coercions applied to them.
;   :FUNCALL		The value will be called as a function.
; See ZCPRIM>STANDARD-COERCIONS, ZCPRIM>STANDARD-UNARY-COERCIONS, and
; ZCPRIM>STANDARD-BINARY-COERCIONS for more details.  These symbols can potentially
; (though this didn't turn out to be useful) appear together -- so the way to test,
; for instance, if the value of the expression being expanded is going to be used
; as a boolean is to say (MEMQ ':BOOLEAN (CAR **CONTEXT)).  I.e., **CONTEXT is a
; list of lists.  However, a couple of symbols, when they appear in a CAR of a
; frame (an element of **CONTEXT), mean that that entire entry is interpreted
; specially.  These are:
;   :LOCALS		The CDR is a list whose CARs are local variables declared in
;				this context (the CADR is the type, and the CADDR the initial
;				value, if any).
;   :D-BLOCK		This expression is contained within a block (other than the
;				top-level block of the function) that declares some local
;				variables.  The CDR of this context frame contains information
;				used by setjmp (q.v.).
;   :DEFUNC		This frame describes the function being compiled.  See
;				ZCPRIM>FUNCTION-NAME ff. for details.
;
; BTW you probably don't want to change anything in this file; very little in it
; is specific to C.

(defmacro defprim (name &body bodies)
  "Defines a Zeta-C primitive.  The syntax is
     (defprim <name>
       /"Documentation./"
       (<arg-pattern> <body>)
       (<arg-pattern> <body>) ...)
   See zcmac>expand-each-arg for the structure of the arg-patterns.
   DEFPRIM defines a macro that tests its form against each of the <arg-pattern>s,
   and when one matches, executes the appropriate <body>.  The macro expander
   function is put on the ZCMAC>EXPAND property of the NAME."
  (let ((documentation (if (stringp (car bodies)) (car bodies)))
	   (bodies (if (stringp (car bodies)) (cdr bodies) bodies)))
    `(progn
	  'compile
	  (macro ,name (form &optional ignore)
		    ,@(if documentation `(,documentation))
		    (funcall (get ',name 'zcmac>expand) form nil nil))
	  ,(zcmac>defprim-tell-zmacs name bodies)
	  . ,(zcmac>prim-expander name bodies))))

(defun zcmac>defprim-tell-zmacs (name bodies)
  "Tells ZMacs how to indent forms calling this primitive.  Only works on the first
   argument pattern given."
  (let ((bodyarg (zcmac>prim-body-arg (caar bodies))))
    (if bodyarg
	   `(eval-when (compile load)
		  #-Genera (set-in-alist zwei:*lisp-indent-offset-alist*
							',name ',(list bodyarg 1))
		  #+Genera (puthash ',name ',(list bodyarg 1)
						zwei:*lisp-indentation-offset-hash-table*)))))

(defun zcmac>prim-body-arg (arglist)
  "Finds the argument number of the &body argument, if any; else returns nil."
  (zcmac>prim-body-arg-1 arglist 0))

(defun zcmac>prim-body-arg-1 (arglist iarg)
  (cond ((null arglist) nil)
	   ((eq (car arglist) '&body) iarg)
	   ((memq (car arglist) '(&optional &rest))
	    (zcmac>prim-body-arg-1 (cdr arglist) iarg))
	   ((memq (car arglist) '(&quote &eval))
	    (zcmac>prim-body-arg-1 (cdr arglist) iarg))
	   (t (zcmac>prim-body-arg-1 (cdr arglist) (1+ iarg)))))

(defun zcmac>prim-expander (name bodies)
  "Generates a function to expand (translate from Zeta-C to Lisp) a call to a
   Zeta-C primitive."
  `((defun (:property ,name zcmac>expand) (**form **env **context)
	 (in-area zc-temporary-area
	   (let ((**args (cdr **form)))
		(ignore **form **env **context **args)		 ; to suppress warnings.
		(cond ,@(zcmac>expand-variations bodies)
			 (t (zcerror "Wrong number of arguments, ~D, to Zeta-C primitive ~A: ~A"
					   (length (cdr **form)) (car **form) **form))))))))

(defun zcmac>expand-variations (bodies)
  "For each of several variations on a primitive (defined by argument number), generates
   code to translate that kind of call."
  (mapcar #'(lambda (variation)
		    (let ((arglist (car variation))
				(body (cdr variation)))
			 `(,(zcmac>arglist-check arglist)
			   . ,(zcmac>args-expander arglist (zcmac>massage-args arglist body)))))
		bodies))

(defun zcmac>arglist-check (arglist)
  "Generates a form to determine whether the arglist matches the number of args actually
   given in a call to a primitive."
  `(let ((nargs (length **args)))
	,(zcmac>arglist-check-1 arglist 0 nil)))

(defun zcmac>arglist-check-1 (arglist iarg optional-p)
  (cond ((null arglist) `(,(if optional-p '<= '=) nargs ,iarg))
	   ((eq (car arglist) '&optional)
	    `(and (>= nargs ,iarg)
			,(zcmac>arglist-check-1 (cdr arglist) iarg t)))
	   ((memq (car arglist) `(&rest &body))
	    (if optional-p t `(>= nargs ,iarg)))
	   ((memq (car arglist) '(&quote &eval))
	    (zcmac>arglist-check-1 (cdr arglist) iarg optional-p))
	   (t (zcmac>arglist-check-1 (cdr arglist) (1+ iarg) optional-p))))

(defun zcmac>args-expander (args body)
  "Generates forms to bind the args to the translations of their respective
   arguments and types, and wraps them around the body."
  (if (eq (car body) ':alias-for)
	 `(funcall #',(get (cadr body) 'zcmac>expand) **form **env **context)
    (zcmac>expand-each-arg args nil nil body)))

(defun zcmac>expand-each-arg (args arg-option arg-quoted primbody)
  "Generates a form to bind each argument and type variable to the result of
   translating the corresponding form.  ARGS is a list of the form
     ((arg1 type1 {options...}) {&optional (arg2 type2 ...)}
      {&rest (args types ...)} {&body (body ...)}
      {&quote arg-exp ...})
   &REST is used for functions that take a variable number of arguments; &BODY
   is used for blocks of statements."
  (cond ((null args) primbody)
	   ((and (symbolp (car args))
		    (memq (car args) '(&optional &rest &body)))
	    (zcmac>expand-each-arg (cdr args) (car args) arg-quoted primbody))
	   ((eq (car args) '&quote)
	    (zcmac>expand-each-arg (cdr args) arg-option t primbody))
	   ((eq (car args) '&eval)
	    (zcmac>expand-each-arg (cdr args) arg-option nil primbody))
	   (t (selectq arg-option
		   ((nil &optional)
		    (if (and (not arg-quoted) (not (listp (car args))))
			   (ferror "Must use list: (argname typename)"))
		    (let ((newbody (zcmac>expand-each-arg (cdr args) arg-option
										  arg-quoted primbody)))
			 (if arg-quoted
				`((let ((,(car args) (car **args)) (**args (cdr **args)))
				    (ignore **args) . ,newbody))
			   (zcmac>bind-single-arg (car args) newbody))))
		   ((&rest &body)
		    (if (and (not arg-quoted) (not (listp (car args))))
			   (ferror "Must use list: (argname typename)"))
		    (let ((newbody (zcmac>expand-each-arg (cdr args) '&error arg-quoted primbody)))
			 (if arg-quoted
				`((let ((,(car args) **args)) . ,primbody))
			   (zcmac>bind-rest-arg (car args) (eq arg-option '&rest) newbody))))
		   (&error (zcerror "Only one arg may follow &rest or &body"))
		   (otherwise (zcerror "Internal error: unknown arg-option"))))))

(defun zcmac>bind-single-arg (argspec body)
  "Wraps forms around the body to bind an arg/type pair to the results of translating
   the next argument expression."
  (let ((vars (zcmac>vars-from-argspec argspec t)))
    `((multiple-value-bind ,vars
		(if **args
		    (zcmac>translate-exp (car **args) **env (cons ',(cddr argspec) **context)))
	   (let ((**args (cdr **args)))
		(ignore **args)
		. ,body)))))

(defun zcmac>bind-rest-arg (argspec expressions-p body)
  "Wraps forms around the body to bind either a &rest arg/type pair or a &body arg
   to the results of translating the remaining argument expressions."
  (let ((vars (zcmac>vars-from-argspec argspec expressions-p)))
    `((multiple-value-bind ,vars
		(zcmac>translate-rest-arg **args ',expressions-p **env
							 (cons ',(if expressions-p (cddr argspec) (cdr argspec))
								  **context))
	   . ,body))))

(defun zcmac>bind-context-arg (argspec body)
  "Wraps forms around the body to bind the context arg."
  (let ((vars (zcmac>vars-from-argspec argspec nil)))	 ; for error checking.
    `((let ((,(car vars) **context))
	   . ,body))))

(defun zcmac>vars-from-argspec (argspec expressions-p)
  "Strips the options off a defprim argspec, leaving just the variables to be bound.
   Bugs: doesn't actually look for options; assumes the first 1 or 2 elements of the
   list are the variables."
  (cond ((nlistp argspec)
	    (ferror "Defprim argument must be a list: (argname [typename] {options})"))
	   ((and expressions-p (< (length argspec) 2))
	    (ferror "Type parameter is required for expression arguments"))
	   (t (let ((nvars (if expressions-p 2 1)))
		   (if (> (length argspec) nvars)
			  (firstn nvars argspec)
			  argspec)))))

;; Unlike most of the functions in this file, this one is actually specific to C.
(defun zcmac>massage-args (arglist body)
  "Arranges for post-processing on the args, that is, after they are translated but
   before calling the body to do the actual expansion.  This is used for the standard
   binary conversions."
  (nlet ((bin1 (some arglist #'(lambda (argspec)
						   (and (listp argspec) (memq ':binary (cddr argspec))))))
	    ((bin2 (some (cdr bin1)
				  #'(lambda (argspec)
					 (and (listp argspec) (memq ':binary (cddr argspec))))))
		((bindlist (and bin1 `(,(caar bin1) ,(cadar bin1)
						   ,(caar bin2) ,(cadar bin2)))))))
    (cond ((not bin1) body)
		((not bin2)
		 (ferror "Only one :BINARY argument"))
		(t `((nlet ((,@bindlist (zcprim>standard-binary-coercions . ,bindlist)))
			  . ,body))))))

;; Another C-specific function.
(defun zcmac>translate-exp (exp env context)
  "Translates an expression from C into Lisp, returning two values: the translated
   expression and its type.  Performs standard coercions."
  (multiple-value-bind (exp-trans exp-type)
	 (zcmac>translate-exp-1 exp env context)
    (zcprim>standard-coercions exp-trans exp-type context exp)))
(defun zcmac>translate-exp-1 (exp env context)
  (cond ((null exp) nil)
	   ((symbolp exp)
	    (if (memq ':statement (car context))
		   exp
		 (zcprim>variable-reference exp env context)))
	   ((nlistp exp)
	    (if (memq ':statement (car context))
		   nil					; constant at top level -> NIL
		 (values exp (zctype>constant-type exp))))
	   ((get (car exp) 'zcmac>expand)
	    (funcall (get (car exp) 'zcmac>expand) exp env context))
	   (t (zcprim>trans-misc-fun-call exp env context))))

(defun zcmac>translate-rest-arg (args expressions-p env context)
  (if (null args)
	 nil
    (multiple-value-bind (arg-trans arg-type)
	   (zcmac>translate-exp (car args) env context)
	 (if expressions-p
		(multiple-value-bind (rest-trans rest-type)
		    (zcmac>translate-rest-arg (cdr args) expressions-p env context)
		  (values (cons arg-trans rest-trans)
				(cons arg-type rest-type)))
	   (cons arg-trans (zcmac>translate-rest-arg (cdr args) expressions-p env context))))))


; End of ZCMAC.LISP


