; GMAP, version 3.1, by Scott Layson
;
; This is not the final version!  Suggestions welcome!
;
; The GMAP macro provides a new kind of iteration facility in LISP.
; Basically, it is intended for when you would like to use MAPCAR, but
; can't because the things you want to map over aren't in lists, or you
; need to collect the results of the mapping into something other than a
; list.  That is, GMAP is probably the right thing to use when you are
; using iteration to perform the same computation on each element of
; some collection, as opposed to changing your state in some complicated
; way on every iteration of a loop.
; In fact, it's conceptually reasonable to imagine all the iterations of a
; GMAP as happening in parallel, just as you might with MAPCAR.  The
; difference is that with GMAP you explicitly say, via keywords, what kinds
; of collections the elements come in and what kind of collection to make
; from the result.  For example, the following two expressions are equivalent:
; (mapcar #'foo this-list that-list)	and
; (gmap (:list) #'foo (:list this-list) (:list that-list))
; The first :list keyword indicates that GMAP is to build a list; the other
; two tell it that this-list and that-list are in fact lists of elements over
; which foo is to be mapped.  Other keywords exist besides :list; for
; example, :string, if used as an argument keyword, causes its argument
; to be viewed as a string; the values it "generates" for the function being
; mapped are the successive characters of the string.
; Perhaps the best feature of GMAP is its facility for defining one's own
; keywords.  Thus if one has some funny data structure, in which the
; elements are stored in some wierd way, one can define a keyword for
; it by telling GMAP how to find successive elements.
;
; [Revision of documentation stopped here; please forgive the mess that
; follows.]
;
; The overall syntax of GMAP is:
;   (gmap <result-spec> <fn>
;	  <arg-spec-1>
;	  <arg-spec-2>
;	  ... )
; where <fn> is the function being mapped, just like the first argument
; to MAPCAR.  The <result-spec> and the <arg-spec-i> are lists, whose first
; element is a keyword indicating the type of result constructor or argument
; generator, and the interpretation of whose subsequent elements depends on
; that type.  For example, in:
;   (gmap (:list) #'+
;	  (:list '(14 32 51))
;	  (:index 3))
; #'+ is the function to be mapped;
; the result-type of :list specifies that a list is to be constructed containing
; the results;
; the first arg-spec specifies that the first argument to the function
; being mapped will be successive elements of the list '(14 32 51);
; and the second arg-spec says that the second argument will be successive
; integers starting with 3.
; The result, of course, is (17 36 56).
; [Anyone familiar with CLU will recognize that an arg-spec is something like
; a CLU iterator.]
;
; **** Argument generators ****
; From a useability standpoint, the right way to have implemented these would
; have been with closures [see Section 10.2 of the LispMachine Manual].
; However, to maintain MacLisp compatibility [and for speed], we fake closures
; in a somewhat annoying way: GMAP itself maintains the one state variable that
; each generator is permitted, and we have to tell it explicitly how to get from
; the current value of the state variable to a)the value to be generated and
; b)the next value of the state variable.
;
; The keyword, the first element, of each argument spec tells what kind of
; generator to use.  NIL as a keyword specifies that one is defining a generator
; for this instance of GMAP only instead of using one of the predefined ones.
; A NIL-type arg-spec has the following [somewhat hairy] syntax:
;   (nil <init> &optional <exitp> <argfn> <nextfn> <let-specs>)
; where <init> is the initial value of the generator's state variable;
; <exitp>, if non-nil, is a function of one argument; when it becomes true of
;  [i.e., returns a non-nil value when applied to] the state variable, the
;  iteration terminates.  If it is absent or nil, this generator has no exit-test.
;  If more than one arg-spec supplies an exitp, then the
;  first one to finish terminates the entire iteration [just like mapcar, which
;  stops when any list runs out].
; <argfn>, if non-nil, is a function of one argument which is applied to the
;  current value of the state variable to get the value the generator actually
;  returns on this iteration.
; <nextfn>, if non-nil, is a function of one argument which takes the current
;  value of the state variable and returns the next.
; <let-specs> facilitates arbitrary hair and is explained below.
; For example, an arg-spec of (:list foo) is equivalent to
; (nil foo #'null #'car #'cdr)
; where foo is the initial value of the list; #'null is the predicate that says
; when the list has run out; #'car, the argfn, is what is done to the list to
; get the current element; and #'cdr, the nextfn, is what is done to the list
; to get the next list.
;
; An argument generator described this way is conceptually equivalent to
; (let-closed `(state-var ,@<let-specs>)
;   #'(lambda ()
;	(if (<exitp> state-var)
;	    (*throw 'exit-iteration nil)
;	    (prog1 (<argfn> state-var)
;		   (setq state-var (<nextfn> state-var))))))
;
; Note that if only (nil <init>) is specified, the argument is a constant <init>;
; no more circular-list'ing!
;
; Other predefined argument types include:
;  (:list <list>)
;    As shown in examples above: supplies successive elements of <list>.
;  (:index <start> &optional <stop> <incr>)
;    Provides numbers beginning at <start> and going to (but not including) <stop>
;    incrementing by <incr> each time.  If <stop> is missing or nil, this generates
;    numbers indefinitely.  <incr> may be positive or negative and defaults to 1.
;  (:index-inc <start> <stop> &optional <incr>)
;    "Index, INClusive": like :index, but the numbers generated include <stop>.
;  (:array <array>)
;    [Only works on Lispm]
;    Generates successive elements of <array>.	Uses ARRAY-ACTIVE-LENGTH to
;    figure out how many there are.
;  (:string <string>)
;    [Only works on Lispm]
;    Generates successive characters of <string>.
;
; **** Result Constructors ****
; Again, this could reasonably have been done with closures.
;
; Like arg-specs, result-specs begin with a keyword saying what kind of
; constructor to use, i.e., how to put together the results of the function
; being mapped.  And again, a keyword of NIL means that no predefined
; constructor is being used.  A NIL-type result-spec looks like:
;  (nil <init> <resfn> &optional <cleanup> <filterp> <let-specs>)
; where
; <init> is the initial value of the constructor's state variable;
; <resfn> is a function of two arguments, the current value of the state variable
;  and the current value returned by the function being mapped; it gives the next
;  value of the state variable.
; <cleanup>, if present and non-nil, is a function of one argument that translates
;  the final value of the state variable into the value that the gmap actually returns.
; <filterp>, if present and non-nil, is a predicate of one argument; when it is false
;  of the current value of the function being mapped, <resfn> is not called on that
;  iteration, and the value of the state variable is unchanged.
; <let-specs>, as before, is hairy; I'll get back to it below.
; For example, a res-spec of (:list) is equivalent to
; (nil nil #'xcons #'nreverse)
; -- the state variable starts at nil, gets successive values consed onto it ["xcons"
;  is used because the arguments are in the wrong order for #'cons], and gets
;  nreversed before being returned.
;
; Other predefined result types include:
;  (:list)
;    Generates a list, like mapcar, of the values.
;  (:and)
;    Returns the first nil.
;  (:or)
;    Returns the first non-nil.
;  (:sum)
;    Returns the sum of the values.  E.g., to get sum of products, use
;    (gmap (:sum) #'* ...)
;  (:array <initial-array>)
;    Generates an array of the values.	You supply the initial [presumably empty]
;    array; the values are stored starting with element 0.  If the array has a
;    fill pointer, it is set upon exit to the number of elements stored.  The array
;    itself is returned.
;  (:string &optional <length-guess>)
;    Generates a string from the values.  <length-guess> is the initially allocated
;    string size; it defaults to 20.  #'array-push-extend is used to append each
;    character.
;  (:values &rest <result-specs>)
;    The function being mapped is expected to return as many values as there are
;    result-specs; each value is accumulated separately according to its respective
;    result-spec, and finally, all the result values are returned.  This amounts to
;    several simultaneous independent iterations.
;
; **** User-defined argument and result types ****
; Perhaps the most useful [if not quite the best worked out yet] feature of gmap
; is the provision for the user to define his/her own argument generators and
; result constructors.	For example, if in some program you commonly iterate over
; words in a sentence, or lines in a Zwei buffer, or users currently logged onto
; ITS, then define an argument type ":sentence", or ":interval", or ":its-users".
; And similarly with result-types.  The way this is done [which I'm not yet sure
; is entirely satisfactory] is with the two special forms DEF-GMAP-ARG-TYPE and
; DEF-GMAP-RES-TYPE.  These have syntax like DEFUN:
; (def-gmap-foo-type <name> (<args>)
;   <body>)
; When <name> is seen as the keyword of an arg- or result-spec, and has
; been defined with the appropriate special form, then the function
; #'(lambda (<args>) <body>) is applied to the cdr of the spec; that is,
; the keyword itself has been stripped off.  Whatever this returns is interpreted
; as a nil-type spec, except, again, without the keyword "nil".  For example, the
; arg-type :list is actually defined by
;   (def-gmap-arg-type :list (initial-list)
;     `(,initial-list			; init
;	#'null #'car #'cdr))		; exitp, argfn, and resfn
;
; Lists of what arg- and result-types are defined can be found in the variables
; GMAP-ARG-TYPE-LIST and GMAP-RES-TYPE-LIST.
;
; Now for the promised explanation about let-specs.  Sometimes [indeed, fairly
; often] a user-defined type will want to compute values and bind variables
; other than those automatically provided by the iteration.  For example, the
; index type goes to some trouble to only evaluate its parameters once.  It does
; this by providing a list of specs, i.e., (<var> <value>) pairs, which go into
; a LET that surrounds the entire iteration.  Except, that is, for the following
; hack: if you want several dependent initializations, e.g., you want foo to be
; something hairy and bar to be the cdr of foo, you can indicate the dependence
; by the nesting in list structure of the specs:
; ((foo (something-hairy))
;  ((bar (cdr foo))))
; This will cause a gmap that uses this type to expand into
; (let ((foo (something-hairy)))
;   (let ((bar (cdr foo)))
;     ... [iteration] ...))
; For details, see the NLET macro at the end of this file.  For examples,
; see some of the types defined herein.

; Randomnesses you should know:
; Many arg- and result-specs take optional parameters, which are defined to do
;  something only if both present and non-nil.	By "non-nil" here I mean non-nil
;  *at expansion time*; a variable name whose value happened to be non-nil at
;  runtime would not work.
; The function being mapped can itself be nil, subject of course to the above
;  considerations; in which case the identity function of the first argument is
;  used, and other arguments are ignored.


; The top-level macro.
(defmacro GMAP (res-spec fn &rest arg-spec-list)
  (gmap>expand fn
	       (gmap>res-spec-lookup res-spec)
	       (mapcar #'gmap>arg-spec-lookup arg-spec-list)))

; This does the real work.
(defun GMAP>EXPAND (fn res-specs arg-spec-list)
  (let ((param-list
	  (mapcar #'gmap>param arg-spec-list))
	(result-list (gmap>res>init-clauses res-specs))
	(let-specs (gmap>let-specs arg-spec-list res-specs)))
    (let ((one-value-p (null (cdr result-list)))
	  (fnval-vars (mapcar #'(lambda (ignore) (gensym)) result-list)))
      `(nlet ,let-specs
	 (do (,@param-list
	      ,@result-list)
	     ((or ,@(apply #'append (mapcar #'gmap>param>exit-test	; exit test
					    param-list arg-spec-list)))
	      ,(gmap>res>cleanup res-specs result-list one-value-p))
	   ,(if one-value-p
		(if (car fnval-vars)
		    `(let ((,(car fnval-vars)
			    ,(lexpr-funcall #'gmap>funcall fn
					    (mapcar #'gmap>param>arg
						    param-list arg-spec-list))))
		       (setq ,(caar result-list)
			     ,(gmap>res>next (car res-specs) (caar result-list)
					     (car fnval-vars))))
		  #| Null result spec -- just call the function for effect. |#
		  (lexpr-funcall #'gmap>funcall fn
				 (mapcar #'gmap>param>arg param-list arg-spec-list)))
	      `(multiple-value-bind ,fnval-vars
		   ,(lexpr-funcall #'gmap>funcall fn
				   (mapcar #'gmap>param>arg param-list arg-spec-list))
		 . ,(mapcar #'(lambda (fnval result-pair res-spec)
				`(setq ,(car result-pair)
				       ,(gmap>res>next res-spec (car result-pair) fnval)))
			    fnval-vars result-list res-specs))))))))


; extract the let-specs.
(defun GMAP>LET-SPECS (arg-specs res-specs)
  (nconc (mapcan #'fifth arg-specs) (mapcan #'fifth res-specs)))

; generate the do-variable spec for each argument.
(defun GMAP>PARAM (arg-spec)
  (let ((param-name (gensym))
	(init (first arg-spec))
	(nextfn (fourth arg-spec)))
    `(,param-name
      ,init
      ,@(if nextfn
	    `(,(gmap>funcall nextfn param-name))
	    nil))))

; get the argument to the function being mapped from the do-variable.
(defun GMAP>PARAM>ARG (param arg-spec)
  (let ((param-name (first param))
	(argfn (third arg-spec)))
    (gmap>funcall argfn param-name)))

; get the exit test for the variable.
(defun GMAP>PARAM>EXIT-TEST (param arg-spec)
  (let ((param-name (first param))
	(exitp (second arg-spec)))
    (if exitp
	`(,(gmap>funcall exitp param-name))
	nil)))

; get the initial value of the result.
(defun GMAP>RES>INIT-CLAUSES (res-specs)
  (mapcan #'(lambda (res-spec) (and res-spec (ncons (list (gensym) (first res-spec)))))
	  res-specs))

; compute the next value of the result from the current one and the
; current value of the function.
(defun GMAP>RES>NEXT (res-spec result fnval)
  (let ((resfn (second res-spec))
	(filterp (fourth res-spec)))
    (if filterp
	`(if ,(gmap>funcall filterp fnval)
	     ,(gmap>funcall resfn result fnval)
	     ,result)
	(gmap>funcall resfn result fnval))))

; call the cleanup function on exit.
(defun GMAP>RES>CLEANUP (res-specs result-list one-value-p)
  (if one-value-p
      (gmap>funcall (third (car res-specs)) (caar result-list))
    `(values . ,(mapcar #'(lambda (res-spec result-pair)
			    (gmap>funcall (third res-spec) (car result-pair)))
			res-specs result-list))))

; For some reason, the compiler doesn't convert, e.g., (funcall #'car foo)
; to (car foo); thus we lose some efficiency for functions that would normally
; open-code, like car.	[I think that that optimization would lose if the
; function turned out to be a macro.  If this is right, then one should be
; very careful about using macros as functional args to gmap.]	Hence this
; function to perform the optimization for it.
(defun GMAP>FUNCALL (function &rest args)
  (let ((args (copylist args)))
    (cond ((or (null function) (eq function ':id))
	   (car args))
	  ((and (listp function)
		(eq (car function) 'function))
	   `(,(cadr function) . ,args))
	  (t `(funcall ,function . ,args)))))



(defvar gmap-arg-type-list nil
  "A list of all GMAP arg types that have been defined.")

(defvar gmap-res-type-list nil
  "A list of all GMAP result types that have been defined.")

; define an arg-type.
(defmacro def-gmap-arg-type (name args &body body)
  `(progn
     'compile
     (defun (:property ,name :gmap-arg-spec-generator) ,args . ,body)
     (eval-when (eval load)
       (if (not (memq ',name gmap-arg-type-list))
	   (push ',name gmap-arg-type-list)))))

; define a result-type.
(defmacro def-gmap-res-type (name args &body body)
  `(progn
     'compile
     (defun (:property ,name :gmap-res-spec-generator) ,args . ,body)
     (eval-when (eval load)
       (if (not (memq ',name gmap-res-type-list))
	   (push ',name gmap-res-type-list)))))

; look up an arg type.
(defun gmap>arg-spec-lookup (raw-arg-spec)
  (let ((type (car raw-arg-spec)))
    (if (null type)
	(cdr raw-arg-spec)
	(let ((generator (get type ':gmap-arg-spec-generator)))
	  (if generator
	      (apply generator (cdr raw-arg-spec))
	      (ferror nil "Argument spec, ~S, to gmap is of unknown type
  (Do you have the package right?)")
		      raw-arg-spec)))))

; look up a result type.
(defun gmap>res-spec-lookup (raw-res-spec)
  (if (eq (car raw-res-spec) ':values)
      (mapcar #'gmap>res-spec-lookup-1 (cdr raw-res-spec))
    (ncons (gmap>res-spec-lookup-1 raw-res-spec))))
(defun gmap>res-spec-lookup-1 (raw-res-spec)
  (let ((type (car raw-res-spec)))
    (if (null type)
	(cdr raw-res-spec)
      (let ((generator (get type ':gmap-res-spec-generator)))
	(if generator
	    (apply generator (cdr raw-res-spec))
	  (ferror nil "Result spec, ~S, to gmap is of unknown type
  (Do you have the package right?)"
		  raw-res-spec))))))



; ******** Predefined argument types ********
; See above for documentation.

(def-gmap-arg-type :LIST (initial-list)
  `(,initial-list
    #'null #'car #'cdr))

(def-gmap-arg-type :INDEX (start &optional stop incr)
  (let ((incr-temp (gensym))
	(stop-temp (gensym))
	(bounds-fn-temp (gensym)))
    `(,start					; init
      ,(if stop                                 ; exitp
	   (if incr
	       `#'(lambda (val)
		    (funcall ,bounds-fn-temp val ,stop-temp))
	     `#'(lambda (val) (>= val ,stop-temp)))
	 'nil)
      nil					; no argfn
      ,(if incr                                 ; nextfn
	   `#'(lambda (val) (+ val ,incr-temp))
	   '#'1+)
      (,@(if incr				; and let-specs
	     `((,incr-temp ,incr)
	       ((,bounds-fn-temp (if (minusp ,incr-temp) #'<= #'>=)))))
       ,@(if stop
	     `((,stop-temp ,stop)))))))

(def-gmap-arg-type :INDEX-INC (start &optional stop incr)
  (let ((incr-temp (gensym))
	(stop-temp (gensym))
	(bounds-fn-temp (gensym)))
    `(,start					; init
      ,(if stop                                 ; generate (possibly hairy) exitp
	   (if incr
	       `#'(lambda (val)
		    (funcall ,bounds-fn-temp val ,stop-temp))
	       `#'(lambda (val) (> val ,stop-temp)))
	   'nil)
      nil					; no argfn
      ,(if incr                                 ; nextfn
	   `#'(lambda (val) (+ val ,incr-temp))
	   '#'1+)
      (,@(if incr				; and let-specs
	     `((,incr-temp ,incr)
	       ((,bounds-fn-temp (if (minusp ,incr-temp) #'< #'>)))))
       ,@(if stop
	     `((,stop-temp ,stop)))))))

(def-gmap-arg-type :ARRAY (array)
  (let ((array-temp (gensym))
	(stop (gensym)))
    `(0
       #'(lambda (i) (>= i ,stop))
       #'(lambda (i) (aref ,array-temp i))
       #'1+
       ((,array-temp ,array)
	((,stop (array-active-length ,array-temp)))))))

; This is like :ARRAY but coerces the object to a string first.
(def-gmap-arg-type :STRING (string)
  (let ((string-temp (gensym))
	(stop (gensym)))
    `(0
       (lambda (i) (>= i,stop))
       (lambda (i) (aref ,string-temp i))
       #'1+
       ((,string-temp (string ,string))
	((,stop (string-length ,string-temp)))))))

(def-gmap-arg-type :STREAM (stream &optional (tyi-message ':tyi)
					     (tyipeek-message ':tyipeek))
  `(,stream
    (lambda (s) (null (send s ',tyipeek-message)))
    (lambda (s) (send s ',tyi-message))))


; ******** Predefined result types ********

(def-gmap-res-type :LIST (&optional filterp)
  `(nil #'xcons #'nreverse ,filterp))

(def-gmap-res-type :NCONC (&optional filterp)
  (let ((result-var (gensym)))			; have to use our own, sigh.
    `((locf ,result-var)			; init
       (lambda (next-loc new)			; nextfn
	   (rplacd next-loc new)
	   (if new (locf (cdr (last new))) next-loc))
       (lambda (ignore) ,result-var)
       ,filterp
       ((,result-var nil)))))

(def-gmap-res-type :AND ()
  '(t (lambda (ignore new)
        (if new new (return nil)))))

(def-gmap-res-type :OR ()
  '(nil (lambda (ignore new)
          (if new (return new) 'nil))))

(def-gmap-res-type :SUM ()
  '(0 #'+))

(def-gmap-res-type :MAX ()
  '(nil #'max-with-nil-id))

(defun max-with-nil-id (x y)
  (if (null x) y
    (if (null y) x
      (max x y))))

(def-gmap-res-type :MIN ()
  '(nil #'min-with-nil-id))

(defun min-with-nil-id (x y)
  (if (null x) y
    (if (null y) x
      (min x y))))

(def-gmap-res-type :ARRAY (initial-empty-array)
  (let ((array-temp (gensym)))
    `(0						; init
       (lambda (curr-index next-elt)		; nextfn
         (aset next-elt ,array-temp curr-index)
         (1+ curr-index))
       (lambda (last-index)			; cleanup
         (if (array-has-leader-p ,array-temp)
             (store-array-leader last-index ,array-temp 0))
         ,array-temp)
       nil					; filterp
       ((,array-temp ,initial-empty-array)))))	; let-specs

(def-gmap-res-type :STRING (&optional (length-guess 20.))
  `((make-array nil 'art-string			; init
		,length-guess
		nil '(0))
    #'(lambda (string char)			; nextfn
	(array-push-extend string char)
	string)))



; ================================================================
; A variation on the let macro.  It is entirely upward compatible with the
; standard version; in addition, it cleans up nested lets.  Also, it
; generates multiple-value-bind forms if more than one variable appears
; in a clause (all but the final sexp are taken to be variables).  An example:
; The following two sexps are equivalent:
; (let (foo
;	(bar (car cruft)))
;   (multiple-value-bind (quux zot)
;       (disyfer bar)
;     (do-something-with foo bar quux zot)))
;
; and
;
; (nlet (foo
;	 (bar (car cruft))
;	 ((quux zot (disyfer bar))))
;   (do-something-with foo bar quux zot)))
;
; The idea is that every let-parameter at a depth deeper than one in the
; parameter list assumes the binding of everything at lesser depth.
; Note that there's no guarantee that the value forms will be evaluated in any
; particular order; but there is a guarantee that at each level, all the bindings
; are done in parallel.

(defmacro NLET (params &body body)
  (car (nlet>expand params body)))

(defun nlet>expand (params body)
  (multiple-value-bind (this-level-single this-level-multiple next-level)
      (nlet>split-level params nil nil nil)
    (nlet>expand-1 this-level-single this-level-multiple next-level body)))
(defun nlet>expand-1 (this-level-single this-level-multiple next-level body)
  (cond ((and this-level-multiple
	      (null (cdr this-level-multiple))
	      (null this-level-single))
	 `((multiple-value-bind ,(butlast (car this-level-multiple))
	       ,(car (last (car this-level-multiple)))
	     . ,(nlet>expand-1 nil nil next-level body))))
	(this-level-multiple
	 (let ((vars (butlast (car this-level-multiple))))
	   (let ((gensyms (mapcar #'(lambda (ignore) (gensym)) vars)))
	     `((multiple-value-bind ,gensyms
		   ,(car (last (car this-level-multiple)))
		 . ,(nlet>expand-1 (nconc (mapcar #'list vars gensyms) this-level-single)
				   (cdr this-level-multiple) next-level body))))))
	(this-level-single
	 (multiple-value-bind (ignores real-clauses)
	     (nlet>npartition-list #'(lambda (clause)
				       (and (listp clause) (eq (car clause) 'ignore)))
				   this-level-single)
	   `((progn ,@(mapcar #'cadr ignores)
		    (let ,real-clauses
		      . ,(nlet>expand-1 nil nil next-level body))))))
	(next-level
	 (nlet>expand next-level body))
	(t body)))

(defun nlet>split-level (params this-level-single this-level-multiple next-level)
  (if (null params)
      (values (nreverse this-level-single) (nreverse this-level-multiple)
	      next-level)
      (let ((param (car params)))
	(cond ((and (listp param) (listp (car param)))
	       (nlet>split-level (cdr params) this-level-single this-level-multiple
				 (append next-level param)))
	      ((and (listp param) (cddr param))
	       (nlet>split-level (cdr params) this-level-single
				 (cons param this-level-multiple) next-level))
	      (t
	       (nlet>split-level (cdr params) (cons param this-level-single)
				 this-level-multiple next-level))))))

; DEL-IF and DEL-IF-NOT combined.  I'm surprised this doesn't already exist.
(defun nlet>npartition-list (pred list)
  "Destructively partitions LIST according to PRED.  Returns two values: a list
   of the elements of LIST that satisfy PRED, and a list of those that do not satisfy
   PRED.  LIST becomes one of those two, depending on the fate of its first element."
  (let ((true-list nil)
	(false-list nil))
    (do ((true-loc (locf true-list))
	 (false-loc (locf false-list))
	 (list list (cdr list)))
	((null list)
	 (rplacd true-loc nil)
	 (rplacd false-loc nil)
	 (values true-list false-list))
      (cond ((funcall pred (car list))
	     (rplacd true-loc list)
	     (setq true-loc list))
	    (t
	     (rplacd false-loc list)
	     (setq false-loc list))))))

; A utility to return the list of variables bound by some NLET-clauses.
(defun nlet>bound-vars (clauses)
  "Given a list of NLET-clauses, returns a list of the variables bound by the clauses."
  (and clauses
       (nconc (let ((clause (car clauses)))
		(cond ((symbolp clause) (and (neq clause 'ignore) (ncons clause)))
		      ((symbolp (car clause)) (delq 'ignore (butlast clause)))
		      (t (nlet>bound-vars clause))))
	      (nlet>bound-vars (cdr clauses)))))


(defmacro bcond (&rest clauses)
  "A form of COND that allows for variable binding in the clauses.  Any clause whose
   caar is a symbol is treated as a normal COND clause.  Additionally, clauses may have
   the form
     (((var-1 ... var-n exp) pred)
      consequent consequent ...)
   The <var-i> are bound to the values of <exp>.  <pred> is then evaluated; if the result
   is non-NIL, the <consequent>s are evaluated, and the value(s) of the last one returned,
   in the usual way.  Evaluation of the <pred> and the <consequent>s takes place in an
   environment that includes the bindings of the <var-i>.  This scoping rule is the same,
   by the way, as that used in the bodies of Prolog rules, which is what inspired this
   construct.

   There is an even more general form which a BCOND clause may take:
     (((nlet-clause nlet-clause ...) pred)
      consequent consequent ...)
   Here, the caar of the BCOND clause is a list of binding clauses which are handed to
   NLET, q.v."
  `(prog t () . ,(mapcar #'bcond-clause clauses)))

(defun bcond-clause (clause)
  (cond ((nlistp clause)
	 (ferror nil "The atom ~S is not a valid BCOND clause." clause))
	((or (nlistp (car clause)) (nlistp (caar clause)))
	 #| Normal COND-like clause. |#
	 (bcond-build-clause nil (car clause) (cdr clause)))
	((nlistp (caaar clause))
	 (bcond-build-clause (ncons (caar clause)) (cadar clause) (cdr clause)))
	(t
	 (bcond-build-clause (caar clause) (cadar clause) (cdr clause)))))

(defun bcond-build-clause (nlet-clauses pred consequents)
  (let ((body (if consequents
		  `(cond (,pred (return-from t (progn . ,consequents))))
		(let ((temp-var (gensym)))
		  `(let ((,temp-var ,pred))
		     (cond (,temp-var (return-from t ,temp-var))))))))
    (if nlet-clauses
	`(nlet ,nlet-clauses ,body)
      body)))


; ================================================================
; Finally, we globalize the symbols that need to be accessible.

(globalize 'gmap)
(globalize 'nlet)
(globalize 'bcond)

; Necessary for the Common Lisp package system.
(eval-when (load)
  (export (intern "GMAP" 'global) 'global)
  (export (intern "NLET" 'global) 'global)
  (export (intern "BCOND" 'global) 'global))

; End of GMAP.LISP
