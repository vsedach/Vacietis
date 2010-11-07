; -*- Mode: Lisp; Package: Zwei; Base: 10; Syntax: Zetalisp -*-
; File: ZCZWEI-SYM.LISP
;
;    This code has been placed in the public domain.
;
; This file contains C mode for ZMACS, as implemented for Symbolics Release 7.
;
; 7/02/87 Devon merged in improvements from zczwei-lmiti.lisp
;		which see for list of known bugs, etc.

(defvariable *C-block-indentation* nil :fixnum-or-nil
  "The distance, in spaces, to indent nested C blocks.  If NIL, defaults to the
   current tab width.")

(defvariable *C-indent-}-as-outside* t :boolean
  "If T, a } is lined up with the statements *outside* the block it closes; if NIL,
   with the *inside* of the block.")

; DEFVARIABLE doesn't do this automatically!
; These are symbol macros, on which SETQ barfs but SET doesn't
(set '*C-block-indentation* nil)
(set '*C-indent-}-as-outside* t)

(defflavor c-syntax-mixin () ()
  (:required-flavors major-mode))

(defmethod (:all-uppercase c-syntax-mixin) () nil)

;; This may be made to work someday...  See (:method file-buffer-mixin :save)
;; (in sys: zwei; zmacs).
(defmethod (:check-parens-when-saving c-syntax-mixin) () nil)

(defmethod (:additional-attributes c-syntax-mixin) ()
  '((:package "Package" "~A")
    (:comments-nest "Comments-Nest" "~A")))	;Is this right???

(defprop :comments-nest t fs:known-file-attribute)
(cl:pushnew :comments-nest *MODE-LINE-PROPERTIES*)
(cl:pushnew :comments-nest *ATTRIBUTES-REMEMBERED-IN-BUFFER*)

;;; :COMMENTS-NEST doesn't work, due to some bug in FORWARD-COB no doubt???

(defflavor c-syntax-mode-forms-mixin () (c-syntax-mixin))

(defmethod (:mode-forms c-syntax-mode-forms-mixin) ()
  '((set-char-syntax word-alphabetic *mode-word-syntax-table* #/_)
    (set-comtab *mode-comtab*
		'(#\Tab com-Indent-for-C			; #\Line calls this automatically!
		  ;;#\c-m-T com-Exchange-Cobs
		  ;;#\c-m-@ com-Mark-Cob
		  #\c-m-F com-Forward-Cob
		  #\c-m-B com-Backward-Cob
		  ;;#\c-m-K com-Kill-Cob
		  ;;#\c-m-Rubout com-Backward-Kill-Cob
		  #\c-m-/) com-Forward-Up-Cob
		  #\c-m-/( com-Backward-Up-Cob
		  #\c-m-U com-Backward-Up-Cob
		  #\c-m-D com-Forward-Down-Cob
		  #\c-m-Q com-Indent-Region-for-C		; in lieu of -Block-
		  #\c-m-A com-Beginning-of-C-Function-or-Declaration
		  #\c-m-E com-End-of-C-Function-or-Declaration
		  #\c-m-H com-Mark-C-Function-or-Declaration
		  #\c-m-R com-Reposition-Window-for-C
		  #\m-/; com-End-Comment
		  #\c-sh-A com-C-quick-arglist			;com-quick-arglist wired for lisp
		  #\Break com-Break-for-C
		  )
		'(("Set Package" . com-Set-C-Package)))
    (setq *space-indent-flag* t)
    (setq *paragraph-delimiter-list* nil)
    (setq *comment-start* "//*")
    (setq *comment-begin* "//*")
    (setq *comment-end* "*//")
;   (setq *flash-matching-paren* nil)		; tries to parse Lisp!???
    ;; Try to make at least some of the existing Lisp parsing work.
    ;; This appears to be sufficient to get matching brackets and braces to
    ;; work correctly, most of the time anyway.
    ;; There may be full-blown C parsing someday...
    (set-char-syntax list-alphabetic	*mode-list-syntax-table* #/_)
    (set-char-syntax list-double-quote	*mode-list-syntax-table* #/')
    (set-char-syntax list-alphabetic	*mode-list-syntax-table* #//)
    (set-char-syntax list-delimiter	*mode-list-syntax-table* #/|)
    (set-char-syntax list-slash		*mode-list-syntax-table* #/\)
    (set-char-syntax list-delimiter	*mode-list-syntax-table* #/,)
    (set-char-syntax list-delimiter	*mode-list-syntax-table* #/;)
    (set-char-syntax list-open		*mode-list-syntax-table* #/[)
    (set-char-syntax list-close		*mode-list-syntax-table* #/])
    (set-char-syntax list-open		*mode-list-syntax-table* #/{)
    (set-char-syntax list-close		*mode-list-syntax-table* #/})))

(defflavor c-language-mixin () (c-syntax-mixin))

(defmethod (:definition-interval c-language-mixin) (bp &optional comments-p pkg)
  (nlet ((interval start-bp (c-object-interval (beg-line bp) 1 nil
					       comments-p comments-p))
	 (package (or pkg package)))
    (if (null interval)
	(values nil nil "No definition or declaration here")
      (check-interval-sections interval)
      (nlet ((name type (send self :section-name (bp-line start-bp))))
	(values interval
		(format nil "~A ~A"
			(if (eq type 'defun) "Function" "Declaration of")
			name)
		nil)))))

(defmethod (:default-definition-region c-language-mixin) (point &optional no-error)
  (nlet ((interval name error-p (send self :definition-interval point)))
    (if error-p
	(and (not no-error) (barf error-p))
      (values interval name))))

;Returns definition name, definition type, and interval
;Signals BARF if cannot find the definition; could be a more specific condition...
(defmethod (:definition-around-bp c-language-mixin) (bp)
  (declare (values fspec type section))
  (nlet ((interval start-bp (c-object-interval (beg-line bp) 1 nil nil)))
    (if (null interval)
	(barf "Unable to find the top-level definition surrounding the cursor.")
      (check-interval-sections interval)
      (nlet ((name type ignore error-p
		    (send self :section-name (bp-line start-bp))))
	(when (or error-p
		  (null (get type 'si:definition-type-name)))
	  (barf "Can't find the name of this definition."))
	(let ((section (line-node (bp-line start-bp))))
	  (values name type
		  (if (and (typep section 'section-node)
			   (equal name (send section :function-spec))
			   (equal type (send section :definition-type)))
		      section interval)))))))

(defmethod (:compiler-function c-language-mixin) ()
  #'compiler:compile-to-core)

(defmethod (:compilation-supported c-language-mixin) () t)

(defmethod (:read-interval-stream c-language-mixin) (bp1 &optional bp2 in-order-p)
  (send self :check-package)
  (get-interval bp1 bp2 in-order-p)
  (nlet ((int (bp-top-level-node bp1))
	 ((pathname (send-if-handles int :pathname))))
    (zeta-c:make-c-parser (open-interval-stream bp1 bp2 in-order-p)
			  pathname
			  :start-line (count-lines (interval-first-bp int) bp1 t)
			  :whole-file (and pathname
					   (bp-= (interval-first-bp int) bp1)
					   (bp-= (interval-last-bp int) bp2)))))

(defmethod (:evaluation-supported c-language-mixin) () t)

(defmethod (:initial-sectionization-state c-language-mixin) () nil)

(defmethod (:default-compiler-object-file-type c-language-mixin) ()
  si:*default-binary-file-type*)

(defmethod (:default-source-file-type c-language-mixin) () :c)

(defmethod (:compile-to-file-function c-language-mixin) ()
  #'zeta-c:c-compile-file)

(defflavor c-mode () (c-syntax-mode-forms-mixin c-language-mixin major-mode))

(defmode com-C-Mode c-mode "Provides commands for editing C code." :C)

(set-comtab *standard-comtab*
	    ()
	    '(("C Mode" . com-C-Mode)))

(let ((pair '(:c . :c)))
  (unless (memq pair fs:*file-type-mode-alist*)
    (push pair fs:*file-type-mode-alist*)
    (push '(:h . :c) fs:*file-type-mode-alist*)))


; ================================================================
; Some utility functions.
; These were taken from ZCPARSE.LISP so that one could at least invoke C mode
; without having the rest of ZETA-C loaded.

(defun c-symbol-start-char-p (c)
  (or (both-case-p c)
      (char-equal c #/_)))

(defun c-symbol-char-p (c)
  (or (c-symbol-start-char-p c)
      (digit-char-p c)))

(defun c-symbol-from-string (string pkg &optional (start 0) (end nil))
  "Extracts and interns a C symbol from a string, starting at START, and ending at
   END, the first non-symbol character, or the end of the string, whichever comes
   first.  NIL if there is no symbol starting at START.  Returns two values: the
   symbol, and the index of the next character after it."
  (and (c-symbol-start-char-p (aref string start))
       (do ((index (1+ start) (1+ index))
	    (length (string-length string)))
	   ((or (and end (>= index end))
		(>= index length)
		(not (c-symbol-char-p (aref string index))))
	    (values (c-intern-symbol (nsubstring string start index) pkg)
		    index)))))

; This started as a copy of zeta-c:zclex>intern.
(defun c-intern-symbol (name &rest options)
  "Interns a symbol in the current package.  Meaningful OPTIONS include :UPCASE,
   which causes conversion to upper case, and :SOFT, which causes the symbol not
   to be interned if not present.  '$' is interpreted as the package 
   prefix delimiter."
  (when (memq ':upcase options)
    (string-upcase name 0 nil nil))
  (nlet (($pos (string-search-char #/$ name))
	 ((pkg
	    (and $pos (pkg-find-package (string-upcase (nsubstring name 0 $pos))
					:find package)))
	  ((pkg name (if (and pkg (neq pkg package))
			 (values pkg (nsubstring name (1+ $pos)))
			 (values package name))))))
    (if (memq :soft options)
	(intern-soft name pkg)
	(intern name pkg))))

(defmethod (:check-package c-syntax-mixin) ()
  (unless (zeta-c:c-package-p package)
    (setq package zeta-c:*c-user-package*)
    (send *interval* :putprop zeta-c:*c-user-package* ':package)))

; Takes structure from ZETA-C:ZCLSTN>VERIFY-C-PACKAGE.
(defcom com-set-c-package "Selects a new C package for this buffer.
Asks for a new package name.  If the package already exists, verifies that it's
a C program package (inherits from C:); otherwise, offers to create it.  Also, if
ZWEI:*SET-ATTRIBUTE-UPDATES-LIST* is :ASK (the default), offers to update the buffer's
attribute list." ()
  (si:sort-aarray si:*package-name-aarray*)
  (do (tpkg)
      ((and tpkg (zeta-c:c-package-p tpkg))
       (nlet ((attr-string (pkg-name tpkg))
	      ((attr-value (intern attr-string pkg-keyword-package))))
	 (send *interval* :putprop tpkg ':package)
	 (setq package tpkg)
	 (set-attribute-internal ':package "Package" attr-string attr-value)
	 (invalidate-buffer-sectionization *interval*))
       (must-redisplay *window* dis-text))
    (nlet ((default (and (zeta-c:c-package-p package) (pkg-name package)))
	   ((pkg (read-package-from-mini-buffer "C package for this file//buffer: "
						default 'return))
	    ((pkg name (if (stringp pkg) (values nil pkg)
			 (values pkg (cl:package-name pkg)))))))
      (cond ((null pkg)
	     (and (y-or-n-p (format nil "Create C package ~A? " name))
		  (setq tpkg (zeta-c:create-c-package name))))
	    ((not (zeta-c:c-package-p name))
	     (format standard-output
		     "~A is not a C package (one which uses /"C:/")." name))
	    (t (setq tpkg pkg)))))
  dis-text)


; ================================================================
; Sectionization.

(defmethod (:section-name-trivial-p c-syntax-mixin) () nil)

(defmethod (:quick-definition-line-p c-syntax-mixin) (line)
  "Does it look like this line contains the beginning of a C section?"
  (c-section-p line nil))

(defun c-section-p (line recursive-p)
  "A line is likely to begin a section if it starts with a symbol in column 0, and
   either the previous line does not start with a symbol or the previous line
   contains a semicolon."
  (and (plusp (string-length line))
       (c-symbol-start-char-p (aref line 0))
       (or recursive-p
	   (null (line-previous line))
	   (not (c-section-p (line-previous line) t))
	   (string-search-char #/; (line-previous line)))))

(defmethod (:section-name c-syntax-mixin) (line &optional ignore ignore)
  "Gets the name of the section starting on LINE.  Returns four values: the symbol
   defined, or NIL; the type of the section ('DEFUN or 'DEFVAR); the string which
   appears in the line to specify that symbol or spec; and T if the line doesn't
   begin a section after all."
  (declare (values symbol type string error-p))
  (if (not (c-section-p line nil))
      (values nil nil nil t)
    (do ((sline line)
	 (prev-sline nil sline)
	 (sidx 0 (1+ sidx))
	 (parendepth 0)
	 possible-func-name
	 probable-func-name)
	((null sline) (values nil nil nil t))
      (cond ((>= sidx (string-length sline))
	     (if (and probable-func-name (zerop parendepth))
		 (return (values probable-func-name 'defun (string probable-func-name)))
	       (setq sline (line-next sline))
	       (setq sidx -1)))
	    ((%string-equal sline sidx "//*" 0 2)
	     (nlet ((next-bp gave-up (search (create-bp sline sidx) "*//" nil nil 2)))
	       (if gave-up
		   ;; If couldn't find end of comment in 2 lines, give up
		   ;; (?? is this a reasonable strategy?)
		   (setq sidx (string-length sline))
		 (setq sline (bp-line next-bp))
		 (setq sidx (1- (bp-index next-bp))))))
	    (t
	     (let ((c (code-char (char-code (aref sline sidx)))))
	       (cond ((eql #/( c)
		      (incf parendepth)
		      (setq probable-func-name possible-func-name))
		     ((eql #/) c)
		      (decf parendepth))
		     ((eql #/: c)	; Sigh, it's a label in column 0.
		      (return (values nil nil nil t)))
		     ((and (eql #/{ c)
			   probable-func-name
			   (zerop parendepth))
		      (return (values probable-func-name 'defun
				      (string probable-func-name))))
		     ((and (eql #/{ c)    ; check for struct/union tag.
			   (zerop parendepth)
			   (not (memq possible-func-name 'c:(struct union))))
		      (return (values possible-func-name 'defstruct
				      (string possible-func-name))))
		     ((and (zerop parendepth)
			   (mem #'eql c '(#/, #/; #/= #/[)))
		      (let ((sec-name (or probable-func-name possible-func-name
					  "Definition")))
			(return (values sec-name 'defvar (string sec-name)))))
		     ((c-symbol-start-char-p c)
		      (nlet ((sym next (c-symbol-from-string sline package sidx)))
			(setq sidx (1- next))
			(setq possible-func-name sym))))))))))

; This reads one section ahead of where it's parsing out the section names, so
; the above heuristic (which requires lookahead) will work.
(defmethod (:sectionize-buffer c-syntax-mixin)
	   (first-bp last-bp buffer stream int-stream added-completions)
  (send self :check-package)
  (let ((buffer-tick (or (send buffer ':send-if-handles ':save-tick) *tick*))
	old-changed-sections)
    (tick)
    ;; Flush old section nodes.  Also collect the names of those that are modified, they
    ;; are the ones that will be modified again after a revert buffer.
    (dolist (node (node-inferiors buffer))
      (and (> (node-tick node) buffer-tick)
	   (push (list (section-node-function-spec node)
		       (section-node-definition-type node))
		 old-changed-sections))
      (flush-bp (interval-first-bp node))
      (flush-bp (interval-last-bp node)))
    (do ((line (bp-line first-bp) (line-next line))
	 (limit (bp-line last-bp))
	 (eofflg)
	 (bp (copy-bp first-bp))
	 (function-spec) (definition-type)
	 (str) (int-line)
	 (prev-node-definition-line) (prev-node-start-bp first-bp) (previous-node nil)
	 (node-list nil) (definition-list nil)
	 (state (send self :initial-sectionization-state)))
	(nil)
      (if (or eofflg (not stream))
	  (setq int-line line)
	;; We have a stream; read another line.
	(multiple-value (line eofflg)
	  (let ((default-cons-area *line-area*))
	    (send stream ':line-in line-leader-size)))
	(if line (setq int-line (funcall int-stream ':line-out line))))
      (move-bp bp int-line 0)          ;Record as potentially start-bp for a section
      ;; See if the line is the start of a section
      (when (send self :quick-definition-line-p int-line)
	(let ((end-bp (backward-over-comment-lines bp ':form-as-blank)))
	  (when (and prev-node-definition-line
		     ;; This  prevents crashing if we think we've found a section
		     ;; that's really inside a comment.  Doesn't catch all such
		     ;; cases, only those that would crash.
		     (not (bp-< end-bp prev-node-start-bp))
		     (let (err)
		       (multiple-value (function-spec definition-type str err state)
			 (send self :section-name prev-node-definition-line bp state))
		       (not err)))
	    (push (list function-spec definition-type) definition-list)
	    (when added-completions
	      (section-completion function-spec str added-completions))
	    (setq previous-node
		  (add-section-node prev-node-start-bp end-bp
				    function-spec definition-type
				    prev-node-definition-line buffer previous-node
				    (if (loop for (fspec type) in old-changed-sections
					      thereis (and (eq function-spec fspec)
							   (eq definition-type type)))
					*tick* buffer-tick)
				    buffer-tick))
	    (push previous-node node-list))
	  (setq prev-node-start-bp end-bp)
	  (setq prev-node-definition-line int-line)))
      ;; After processing the last line, exit.
      (when (or eofflg (and (null stream) (eq line limit)))
	;; If reading a stream, we should not have inserted a CR
	;; after the eof line.
	(when stream
	  (delete-interval (forward-char last-bp -1 t) last-bp t))
	;; The rest of the buffer is part of the last node
	(unless (or (send self ':section-name-trivial-p)
		    (null prev-node-definition-line)
		    (let (err)
		      (multiple-value (function-spec definition-type str err state)
			(send self ':section-name prev-node-definition-line bp state))
		      err))
	  (push (list function-spec definition-type) definition-list)
	  (push (add-section-node prev-node-start-bp last-bp
				  function-spec definition-type
				  prev-node-definition-line buffer previous-node
				  (if (loop for (fspec type) in old-changed-sections
					    thereis (and (eq function-spec fspec)
							 (eq definition-type type)))
				      *tick* buffer-tick)
				  buffer-tick)
		node-list)
	  (setf (line-node (bp-line last-bp)) (car node-list)))
	(setf (node-inferiors buffer) (nreverse node-list))
	(return (send buffer ':putprop (nreverse definition-list) 'zmacs-section-list))))))

(defmethod (:backward-over-comment-lines c-syntax-mixin)
	   (bp &optional top-blank-p up-p stop-at-<page>-p stop-at-bp)
  (ignore top-blank-p up-p stop-at-<page>-p stop-at-bp)     ; Simpleminded, but...
  (c-skip-backward-blank-and-comment-lines bp t))


; ================================================================
; Command support routines.

(defun forward-toplevel-c-object (bp &optional (times 1) fixup-p)
  "Return a bp which is forward across TIMES top-level C objects (declarations or
   function definitions) from BP.  If BP is within such an object, that is included
   in the count.  TIMES negative means move backwards.  FIXUP-P non-NIL means if we
   attempt to move over the beginning or end of the buffer, return a bp to there;
   otherwise return NIL.
Note this is intentionally /"stupid/": it does no brace counting or the like but just
   goes on the simplest textual cues, so it will work (assuming the conventions are
   followed) even when semicolons and braces are missing."
  (if (zerop times)
      (copy-bp bp)
    (if (plusp times)
	(or (forward-toplevel-c-object-forward bp times)
	    (and fixup-p (copy-bp (interval-last-bp *interval*))))
      (or (forward-toplevel-c-object-backward bp (- times))
	  (and fixup-p (copy-bp (interval-first-bp *interval*)))))))

; Internal to forward-toplevel-c-object.
(defun forward-toplevel-c-object-forward (bp times)
  (nlet ((next-start-after (find-c-object-start bp t 1 t))
	 ((next-end (c-skip-backward-blank-and-comment-lines next-start-after))))
    (if (bp-< bp next-end)
	#| We were in the middle of an object. |#
      (if (= times 1)
	  next-end
	(let ((next-start-after-n (find-c-object-start next-start-after
						       t (1- times))))
	  (and next-start-after-n
	       (c-skip-backward-blank-and-comment-lines next-start-after-n))))
      #| We were between objects. |#
      (let ((next-start-after-n (find-c-object-start next-start-after t times)))
	(and next-start-after-n
	     (c-skip-backward-blank-and-comment-lines next-start-after-n))))))

; Internal to forward-toplevel-c-object.
(defun forward-toplevel-c-object-backward (bp times)
  (find-c-object-start bp nil
		       (if (and (not (zerop (bp-index bp)))
				(c-section-p (bp-line bp) nil))
			   (1- times) times)))

(defun find-c-object-start (start-bp forwardp times &optional fixup-p)
  "Searches forward or backward for a line on which a C object appears to start.
   TIMES must be non-negative; FORWARDP controls the direction of search.
   Returns NIL if it runs off the end of the buffer."
  (do ((i 0 (1+ i))
       (line (bp-line start-bp)))
      ((or (null line) (>= i times))
       (if line (create-bp line 0)
	 (and fixup-p
	      (copy-bp (if forwardp (interval-last-bp *interval*)
			 (interval-first-bp *interval*))))))
    (do ((this-start-line line))
	((or (null line)
	     (and (neq line this-start-line) (c-section-p line nil))))
      (setq line (if forwardp (line-next line) (line-previous line))))))

(defun c-skip-backward-blank-and-comment-lines (bp &optional comments-only)
  "Skips backward to the beginning of the line just after the last non-blank, non-comment
   line preceding BP.  (BP is assumed not to be in a comment.)  A comment beginning on a
   line with other stuff on it is not skipped over.  If COMMENTS-ONLY, only goes back to
   the beginning of the first comment before BP."
  (let ((prev-thing (backward-over *whitespace-chars* bp)))
    (or (and (bp-= prev-thing (interval-first-bp *interval*))
	     prev-thing)
	(and (looking-at-backward prev-thing "*//")
	     (nlet ((comment-start (or (search prev-thing "//*" t)
				       (barf "Can't find beginning of comment.")))
		    ((prev-thing-on-line (backward-over '(#\Space #\Tab) comment-start))))
	       (and (zerop (bp-index prev-thing-on-line))
		    (c-skip-backward-blank-and-comment-lines prev-thing-on-line
							     comments-only))))
; Then again, maybe preprocessor commands shouldn't count.  (The bug is that only one
; of a matched pair, #if ... #endif or #lisp ... #endlisp, gets picked up.)
;       (and (not comments-only)
;            (let ((beg-line-bp (beg-line prev-thing 0 t)))
;              (and (looking-at beg-line-bp "#")
;                   (c-skip-backward-blank-and-comment-lines beg-line-bp))))
	(if comments-only bp
	  (beg-line prev-thing 1 t)))))

; Similar to DEFUN-INTERVAL (SYS: ZWEI; FOR), q.v.
(defun c-object-interval (bp &optional (times 1) fixup-p (comments-p t)
			  (top-blank-p nil))
  "Return an interval surrounding the top-level C object that BP is within, or NIL.
   If TIMES is > 1, includes additional objects after that one.  COMMENTS-P non-NIL
   means include comments before the object.  TOP-BLANK-P non-NIL along with
   COMMENTS-P means include one blank line (if any) before anything else.
   The second value is a BP to the first line of the object proper (comments etc.
   not included)."
  (declare (values interval definition-line))
  (nlet ((this-obj-end (forward-toplevel-c-object bp 1 t))
	 ((obj-start (forward-toplevel-c-object this-obj-end -1 fixup-p)))
	 ((end (forward-toplevel-c-object this-obj-end (1- times) fixup-p))))
    (and obj-start end
	 (let ((start (if (not comments-p) obj-start
			(c-skip-backward-blank-and-comment-lines obj-start t))))
	   (let ((start (if (and top-blank-p
				 (line-previous (bp-line start))
				 (line-blank-p (line-previous (bp-line start))))
			    (beg-line start -1)
			  start)))
	     (values (create-interval start end)
		     obj-start))))))

; This is quite simple-minded compared to the Lisp version.
(defun indent-interval-for-C (bp1 &optional bp2 in-order-p point-line (comments-p t))
  "Indent all the lines in the specified interval for C.  A line is in the interval
   iff its beginning is included.  If COMMENTS-P is NIL, comments are not readjusted.
   Returns a BP to the end of the interval adjusted.  Normally, blank lines are left
   with no indentation; however, if POINT-LINE is supplied, that line is indented even
   if blank (as the name suggests, this is typically the preferred treatment for the
   line containing point)."
  (get-interval bp1 bp2 in-order-p)
  (interval-lines (bp1 bp2) (start-line stop-line)
    (do ((line start-line (line-next line))
	 (tbp (create-bp start-line 0))
	 (indent-increment (if *C-block-indentation*
			       (* *C-block-indentation* (font-space-width))
			     (* (or (send *interval* ':get ':tab-width) 8)
				(font-space-width))))
	 ;; Special case for empty line at end of buffer.
	 (stop-line (if (and (= (bp-index bp2) 0) (null (line-next (bp-line bp2)))
			     (bp-= bp1 bp2))
			nil stop-line)))
	((eq line stop-line)
	 (if line (move-bp tbp line 0) (interval-last-bp *interval*)))
      (move-bp tbp line 0)
      (indent-bp-adjustment tbp)
      (if (or (null (line-previous line))
	      (and (neq line point-line) (line-blank-p line))
	      (looking-at tbp "#"))
	  (indent-line tbp 0)
	(nlet ((prev-non-blank-line (c-line-previous-non-blank-or-comment line t))
	       ((prev-line-ind (line-indentation prev-non-blank-line))
		(last-thing-bp (c-end-of-real-text prev-non-blank-line))
		((goal-ind (if (and *C-indent-}-as-outside* (looking-at tbp "}"))
			       (max 0 (- prev-line-ind indent-increment))
			     prev-line-ind)))))
	  (cond ((looking-at-backward last-thing-bp "{")
		 (indent-line tbp (+ goal-ind indent-increment)))
		((and (not *C-indent-}-as-outside*)
		      (looking-at-backward last-thing-bp "}"))
		 (indent-line tbp (max 0 (- goal-ind indent-increment))))
		(t (indent-line tbp goal-ind)))
	  (when comments-p (indent-for-comment tbp)))))))

(defun c-line-previous-non-blank-or-comment (line &optional fixup-p)
  "Returns the first non-blank line before LINE which does not begin with a
   comment (or #).  FIXUP-P means, if we get to the first line of the buffer
   and it's blank, return it anyway."
  (do ((line (line-previous line) (line-previous line)))
      ((or (null line)
	   (let ((first-thing (forward-over *blanks* (create-bp line 0))))
	     (and (not (line-blank-p line))
		  (not (looking-at first-thing *comment-start*))
		  (not (looking-at first-thing "#"))))
	   (and fixup-p (null (line-previous line))))
       line)))

(defun c-end-of-real-text (line)
  "Returns a BP that points right after the last non-whitespace, non-comment
   character on LINE."
  (nlet ((comment-start ignore inside-string
			(find-comment-start line t)))
    (backward-over *blanks*
		   (if (and comment-start (not inside-string))
		       (create-bp line comment-start)
		     (create-bp line (string-length line))))))

;;; here zczwei-lmiti.lisp defines c-compile-object-internal, c-compile-buffer,
;;; c-compile-print-interval, c-compile-interval,
;;; c-compile-buffer-changed-functions, ... I wonder why this doesn't.


; ================================================================
; Commands.

(defvar *debug-forward-cob* nil "Flush this variable!")

(defcom com-Forward-Cob "Move one or more C objects forward." (km)
  #+debugging
  (setq *debug-forward-cob* *numeric-arg-p*
	*numeric-arg* 1)
  (move-bp (point)
	   (or (forward-cob (point) *numeric-arg*) (barf)))
  dis-bps)

(defcom com-Backward-Cob "Move one or more C objects backward." (km)
  #+debugging
  (setq *debug-forward-cob* *numeric-arg-p*
	*numeric-arg* 1)
  (move-bp (point)
	   (or (forward-cob (point) (- *numeric-arg*)) (barf)))
  dis-bps)

;(defcom com-Exchange-Cob "Exchange C objects." (??)
;(defcom com-Mark-Cob)
;(defcom com-Kill-Cob)
;(defcom com-Backward-Kill-Cob)

(defcom com-Forward-Up-Cob "Moves up one level of C code (braces parens brackets quotes)" (km)
  (move-bp (point)
	   (or (forward-cob (point) *numeric-arg* nil 1) (barf)))
  dis-bps)

(defcom com-Backward-Up-Cob "Moves up one level of C code (braces parens brackets quotes)" (km)
  (move-bp (point)
	   (or (forward-cob (point) (- *numeric-arg*) nil 1) (barf)))
  dis-bps)

(defcom com-Forward-Down-Cob "Moves Down one level of C code (braces parens brackets quotes)" (km)
  (move-bp (point)
	   (or (barf "not implemented") (barf)))
  dis-bps)

(defcom com-beginning-of-c-function-or-declaration
	"Moves to the beginning of the current C function or declaration." (km)
  (let ((bp (or (forward-toplevel-c-object (point) (- *numeric-arg*)) (barf))))
    (point-pdl-push (point) *window*)
    (move-bp (point) bp))
  dis-bps)

(defcom com-end-of-c-function-or-declaration
	"Moves to the end of the current C function or declaration." (km)
  (let ((bp (or (forward-toplevel-c-object (point) *numeric-arg*) (barf))))
    (point-pdl-push (point) *window*)
    (move-bp (point) bp))
  dis-bps)

(defcom com-mark-c-function-or-declaration
	"Puts point and mark around the current C function or declaration." ()
  (let ((int (or (c-object-interval (point) *numeric-arg* nil t t)
		 (barf))))
    (setf (window-mark-p *window*) t)
    (setq *mark-stays* t)
    (point-pdl-push (point) *window* nil nil)
    (move-bp (point) (interval-first-bp int))
    (move-bp (mark) (interval-last-bp int)))
  dis-bps)

(defcom com-indent-for-C "Indent this line in the current C style.
Numeric argument is the number of lines to indent.
Variables you can set to affect the style:
  *C-block-indentation*:  The distance, in spaces, to indent nested C blocks.
     If NIL, defaults to the current tab width.
  *C-indent-}-as-outside*:  If T, a } is lined up with the statements *outside*
     the block it closes; if NIL, with the *inside* of the block." ()
  (let ((end (indent-interval-for-C (beg-line (point)) (beg-line (point) *numeric-arg* t)
				    nil (bp-line (point)) *numeric-arg-p*)))
    (if *numeric-arg-p*
	(move-bp (point) end)
      (indent-bp-adjustment (point))))
  dis-text)

(defcom com-indent-region-for-C
	"Indents all lines in the region in the current C style.
Variables you can set to affect the style:
  *C-block-indentation*:  The distance in spaces to indent nested C blocks.
     If NIL, defaults to the current tab width.
  *C-indent-}-as-outside*:  If T, a } is lined up with the statements *outside*
     the block it closes; if NIL, with the *inside* of the block." ()
  (region (bp1 bp2)
    (indent-interval-for-C bp1 bp2 t))
  dis-text)

#|| Obviated by zmacs flavorization!
(defcom com-C-compile-region
	"C-compiles the current region or function//declaration.
If there is a region, it is compiled; otherwise, the current or next
function or declaration is compiled." ()
  (c-compile-object-internal t "C-compiling" "compiled.")
  dis-none)
||#

;;; zczwei-lmiti.lisp defines com-C-syntax-check-region here.  Should I???

#|| Obviated by zmacs flavorization!
(defcom com-C-compile-buffer
	"C-compiles the entire current buffer; or, with an argument, the rest of the buffer
/(starting at point)." ()
  (c-compile-buffer t "C-compiling" "compiled." *numeric-arg-p*)
  dis-none)
||#

;;; zczwei-lmiti.lisp defines com-C-syntax-check-buffer here.  Should I???

#|| Obviated by zmacs flavorization!
(defcom com-C-compile-buffer-changed-sections
	"C-compile any sections in this buffer which have been edited.
A numeric arg means ask about each section individually." ()
  (si:file-operation-with-warnings ((and (buffer-file-id *interval*)
					 (send (send *interval* ':generic-pathname)
					       ':generic-pathname))
				    ':compile nil)
    (compiler:compiler-warnings-context-bind
      (c-compile-buffer-changed-functions *interval* *numeric-arg-p*
					  t '("C-compile" "C-compiling" "compiled."))))
  (format t "~&Done.~%")
  dis-none)
||#

;;; it would be nice if this was obviated by flavorization,
;;; funny that it's not necessary on the lmiti zmacs.

(defcom com-c-quick-arglist
	"Displays the argument list for the current C function.
With a numeric argument, it reads the function name from the minibuffer.  " ()
  (quick-c-arglist)
  dis-none)

(defun quick-c-arglist (&optional (stream *typein-window*))
  (if *numeric-arg-p*
      (let ((name (read-function-spec "Arglist" (relevant-function-name (point)))))
	(print-arglist name stream))
      (let ((function (relevant-function-name (point))))
	(when (null function) (barf))				;Looked hard but couldn't find a defined function
	(condition-case ()
	     (print-arglist function stream)
	   (sys:undefined-function
	     ;; If undefined, back off to defun or whatever, if possible
	     (setq function (relevant-function-name (point) nil t nil))
	     (when (null function) (barf))
	     (condition-case ()
		  (print-arglist function stream)
		;; Should this do package dwim?
		(sys:undefined-function (barf "~S is not a defined function" function))))))))

; Modified from COM-REPOSITION-WINDOW in SYS:ZWEI;COME
; Only change is two instances of DEFUN-INTERVAL -> C-OBJECT-INTERVAL.
; Should indirect through major mode!
(defcom com-Reposition-Window-for-C "Try to get all of current C function//declaration in the window.
If function beginning is on the screen,
 scrolls down to bring the end onto the screen.
If function beginning is off screen, scrolls up so beginning appears,
 but will not push point off the bottom.
If function beginning is at the top of the screen,
 tries omitting or including the comments before it.
If function is entirely on screen, positions it at the top
 (or, with numeric arg, at the bottom) of the screen." (KM)
  (let ((point (point))
	(sheet (window-sheet *window*))
	(n-plines (window-n-plines *window*))
	(int (c-object-interval (point) 1 t t)) ; Only change is here
	start-bp end-bp
	recenter-bp)
    (cond ((not (null int))
	   (setq start-bp (interval-first-bp int)
		 end-bp (interval-last-bp int))
	   ;; Don't include the blank line after the defun
	   (and (zerop (bp-index end-bp)) (setq end-bp (end-line end-bp -1 t)))
	   (cond ((and (pline-of-point t *window* start-bp)     ;If start of defun on the screen
		       (null (pline-of-point t *window* end-bp))        ;and not bottom
		       (multiple-value-bind (line index)
			   (put-point-at-pline sheet (bp-line end-bp)
					       (bp-index end-bp) (1- n-plines)
					       (interval-first-bp *interval*)
					       (interval-last-bp *interval*) :line)
			 (setq recenter-bp (create-bp line index))
			 ;; And can fit bottom of the defun on as well
			 ;; then start at the top of the function.
			 (not (bp-< start-bp recenter-bp)))))
		 ((bp-< start-bp
			(setq recenter-bp 
			      (multiple-value-bind (line index)
				  (put-point-at-pline sheet (bp-line point)
						      (bp-index point) (1- n-plines)
						      (interval-first-bp *interval*)
						      (interval-last-bp *interval*)
						      :line)
					    (create-bp line index))))
		  ;; If displaying from the start of the defun would push point off
		  ;; the bottom, complain, and bring in as much as possible anyway.
		  (beep))
		 ;; Start of defun thru point fits on the screen.
		 ((and *numeric-arg-p* (pline-of-point t *window* end-bp))
		  ;; If numeric arg, and end of function is on screen,
		  ;; try putting end of function at bottom.
		  (multiple-value-bind (line index)
		      (put-point-at-pline sheet (bp-line end-bp) (bp-index end-bp)
					  (1- n-plines)
					  (interval-first-bp *interval*)
					  (interval-last-bp *interval*) :line)
		    (setq recenter-bp (create-bp line index))))
		 (t
		  ;; If already at the default place, try including wcomments above the defun.
		  (and (bp-= (window-start-bp *window*) start-bp)
		       (setq start-bp (interval-first-bp (c-object-interval (point) 1 t nil))))
		  (setq recenter-bp start-bp)))
	   (recenter-window *window* ':start recenter-bp))
	  (t (barf "Can't find a C function or declaration here.")))
    dis-none))

(defcom com-Break-for-C "Invokes a C listener on the typeout window." ()
  (let ((*inside-break* t))
    ;; *inside-break* talks to (:method editor-typeout-window :more-exception).
    (zeta-c:c-top-level *typeout-window* t
			(if (typep *interval* 'file-buffer)
			    (send *interval* ':pathname)
			  (fs:default-pathname))))
  (send *typeout-window* ':make-complete)
  dis-none)

#-Genera
(defmethod (:add-patch-interval c-syntax-mixin) (bp1 bp2 in-order-p from-buffer to-buffer)
  (get-interval bp1 bp2 in-order-p)
  ;; If region being patched lies entirely within one section, record that section as patched
  (let ((node (and (eq (bp-node bp1) (bp-node (forward-char bp2 -1 t)))
		   (typep (bp-node bp1) 'section-node)
		   (bp-node bp1)))
	(bp (interval-last-bp to-buffer)))
    ;; Put into the patch buffer, making sure the right package will be used.
    (insert bp (format nil "~%; From file ~A~@[~A~]~%~
			    (GLOBAL:COMPILER-LET ~
			    ((GLOBAL:PACKAGE (GLOBAL:PKG-FIND-PACKAGE /"~A/")))~% ~
			    (ZETA-C:C-SOURCE /"~:*~A/" /"~A/" ~D~2%"
		       (named-buffer-name from-buffer)
		       (send from-buffer :version-string)
		       (send from-buffer :get ':package)
		       (send from-buffer :pathname)
		       (count-lines (interval-first-bp from-buffer) bp1 t)))
    ;; Can't just (insert-interval bp bp1 bp2 t), because slashification needed
    (insert bp (format nil "~S~%"
		       (with-output-to-string (str)
			 (with-open-stream (int (interval-stream bp1 bp2 t))
			   (stream-copy-until-eof int str)))))
    (insert bp (format nil "))~2%"))
    (when node
      (putprop (locf (section-node-plist node)) (node-tick node) 'patch-tick))))

#+Genera
(defmethod (:add-patch-interval c-syntax-mixin) (bp1 bp2 in-order-p from-buffer to-buffer)
  (insert-patch-section-prologue from-buffer to-buffer)
  (get-interval bp1 bp2 in-order-p)
  (let ((node (and (eq (bp-node bp1) (bp-node (forward-char bp2 -1 t)))
		   (typep (bp-node bp1) 'section-node)
		   (bp-node bp1)))
	(bp (interval-last-bp to-buffer))
	(readtable si:*common-lisp-readtable*))
    ;; Put into the patch buffer, making sure the right package will be used.
    (insert bp (format nil "~%(ZETA-C:C-SOURCE /"~A/" /"~A/" ~D~%"
		       (send from-buffer :get ':package)
		       (send from-buffer :pathname)
		       (count-lines (interval-first-bp from-buffer) bp1 t)))
    ;; Can't just (insert-interval bp bp1 bp2 t), because slashification needed
    (insert bp (format nil "~S~%"
		       (with-output-to-string (str)
			 (with-open-stream (int (interval-stream bp1 bp2 t))
			   (stream-copy-until-eof int str)))))
    (insert bp (format nil ")~2%"))
    ;; If patch lies entirely within one section, record that section as patched
    (when node
      (putprop (locf (section-node-plist node)) (node-tick node) 'patch-tick))))

; This fixes a bug whereby patch comments have "*/" appended to them (the variable
; *COMMENT-END*, among others, is still in effect dynamically bound).
(defmethod (:attribute-comment-end lisp-syntax-mixin) ()
  nil)

;;; zczwei-lmiti.lisp defines com-C-compile-file, editor-c-compile-file,
;;; c-file-has-changed here.  Should I???

;========================================================================
;;; go backwards up parentheses until we get a name followed by an open paren.
;;; at top level I want it to scan forwards and then backwards.
;;; maybe at toplevel it should try backward first???

;;; (:method c-syntax-mixin :relevant-function-name) is getting called twice form c-sh-A.
;;; why is this?

(defmethod (:relevant-function-name c-syntax-mixin)
	   (bp stringp function-only funcall-special)
  "Return a function spec obtained from the text around BP.
STRINGP = T says print the spec into a string and return that.
FUNCTION-ONLY means only consider actually defined functions (default T)."
; (DECLARE (VALUES RELEVANT-FUNCTION-NAME BP))
  (declare (special *c-syntax* *reverse-c-syntax*)
	   (ignore funcall-special))
  ;; Look up in list structure to find a function we are in a call to.
  (let ((start-bp (forward-toplevel-C-object bp -1 t))
	(end-bp (forward-toplevel-C-object bp 1 t))
	(toplevel-direction 0)
	toplevel-bp)
    #+debugging (format t "~&Function-Only=~S~&Start-BP ~S~&End-BP ~S~&" function-only start-bp end-bp)
    (do ((bp1					;bp to the end of a candidate
	   ;; might start inside a name, so try this first
	   (forward-c-name bp)
	   ;; thereafter, try going backwards and up
	   (or (forward-cob bp1 -1 nil 1 start-bp nil)
	       ;; can't go up, try going forward
	       (progn
		 (when (= toplevel-direction 0)
		   (setq toplevel-bp bp1
			 toplevel-direction 1))
		 (forward-cob bp1 toplevel-direction nil 0
			      (if (> toplevel-direction 0) end-bp start-bp)))
	       ;; can't go further, try going backward unless we are already
	       (when (> toplevel-direction 0)
		 (setq toplevel-direction -1)
		 toplevel-bp))))		;went forward from here, now try backward.
	;; time to give up?
	((null bp1)
	 nil)
      #+debugging (format t "~&Considering ~S ~C~&" bp1 (char-after-c-whitespace bp1))
      (when (char= (char-after-c-whitespace bp1) #/()
	(let ((bp0 (forward-cob bp1 -1)))
	  #+debugging (format t "~&Found ~S ~C~C ~S~&" bp0 (bp-char-before bp0) (bp-char bp0) (aref *c-syntax* (char-code (bp-char bp0))))
	  (and (eq (bp-line bp0) (bp-line bp1))
	       (eq (aref *c-syntax* (char-code (bp-char bp0))) 'name)
	       (let ((name (substring (bp-line bp0)
				      (bp-index bp0)
				      (bp-index (setq bp1 (forward-c-name bp0))))))
		 #+debugging (format t "<~S intern-soft=~S>" name (intern-soft name))
		 (when (or (not function-only)
			   (c-function-p name))
		   (return (if stringp name (intern name)))))))))))

;;; The arglist displayer doesn't necessarily win if there are callers with
;;; some idea of what the args should be in (get soft 'zeta-c:identifier) but
;;; soft is not a function.
;;;
;;; soft is not a defined function, but it has callers who think its arguments should be...
;;; soft is not a defined function, but there is a definition for it in the file buffer...

(defun c-function-p (name)
  (let ((soft (intern-soft name)))
    (and soft
	 (labels ((function-p (sym)
		    (or (fboundp sym)
			(gmap (:or)
			      #'(lambda (frame)
				  (or (and (eq (car (third frame)) 'zeta-c:type)
					   (eq (cadr (third frame)) ':function))
				      (and (eq (car (third frame))
					       'zeta-c:static-alternate-name)
					   (function-p (cdr (third frame))))))
			      (:list (get sym 'zeta-c:identifier))))))
	   (function-p soft)))))

;;; lmiti sys:zwei;primit.lisp defines a bp-read-object, but I don't see why
;;; I now need to define one since >rel-7>zwei>primit.lisp doesn't have that,
;;; and it's not clear why bp-read-c-name exists anyway???

;;; in slimy charmap-per-line, changed charmap-char to charmap-char
;;; which I hope is right.

(defun forward-c-name (bp)
  "Returns a BP after the last character of a C name"
  (declare (special *c-syntax*))
  (charmap-per-line (bp (interval-last-bp *interval*))
		    ()
    (unless (eq (aref *c-syntax* (char-code (charmap-char))) 'name)
      (charmap-return (charmap-bp-before)))))

(defun char-after-c-whitespace (bp &optional (eof-value #/space)
				&aux (comment-level 0))
  "Returns the first significant C character after BP"
  (declare (special *c-syntax*))
  (charmap-per-line (bp (interval-last-bp *interval*) eof-value)
		    ;;; when memoizing works, line-forms will be useful
		    ()
    (let ((char (charmap-char)))
      (cond
	((eq (aref *c-syntax* (char-code char)) 'space))
	((char= #// char)
	 (if (char= #/* (progn (charmap-increment eof-value)
			   (charmap-char)))
	     (when (send *interval* :get :comments-nest)
	       (incf comment-level))
	   (charmap-return char)))
	((char= #/* char)
	 (if (and (> comment-level 0)
		  (char= #// (progn (charmap-increment eof-value)
				(charmap-char))))
	     (decf comment-level)
	   (charmap-return char)))
	(t
	 (charmap-return char))))))


;;; based on forward-sexp in LPARSE.LISP

;;; being in a hurry, we are extra crude and treat all forms of
;;; parens as equivalent ([{}]) which is a bug of course.

;;; zwei:*c-tokens-to-ignore* should make this this skip over commas, etc.

;;; I don't understand no-up-p and don't have time to figure it out.
;;; It seems like it would be impossible for it to happen, or perhaps
;;; my confusion would go away if it were named no-down-p instead.

;;; removed 3rd arg from (forward-up-string bp (minusp times) nil) because
;;; slimy version doesn't have it.  Does it mean anything???

(defun forward-cob (bp &optional (times 1) fixup-p
		    (paren-level 0) stop-bp no-up-p)
  "Return a bp which is TIMES C objects forward from BP.
TIMES may be negative meaning go backward.  Comments are ignored.
FIXUP-P non-NIL means if go past beginning or end return a bp
 to there; otherwise return NIL in that case.
PAREN-LEVEL > 0 means move up that many levels of list structure.
STOP-BP is a place to give up and return if it is reached;
 the value is NIL or STOP-BP.
NO-UP-P means it is forbidden to move up and then down again.
 NIL is returned if that starts to happen."
  (declare (special *c-syntax* *reverse-c-syntax*))
   (when (c-bp-syntactic-context bp)
     ;; BP is within a string.
     ;; Allow motion over C exps within the string,
     ;; but don't allow motion past boundary of this string.
     ;; Now make an exception for the case of what looks like a defun-beginning
     ;; which is inside a string (such as a |# after the previous defun).
     (unless (and (plusp times)
		  (zerop (bp-index bp))
		  (char= (list-syntax (bp-char bp)) list-open))
       (setq stop-bp (forward-up-string bp (minusp times)))))	;proably buggy for backward case???
   ;; should do the same for BP inside a comment
   #+later??? (when etc etc)
   (cond ((zerop times) (copy-bp bp))
	 ((plusp times)
	  (let ((state 'in-space)	;STATE is one of IN-{ NAME QUOTE OPEN CLOSE SPACE ATSIGN TOKEN }
		(time 0)
		(last-bp (or stop-bp (interval-last-bp *interval*)))
		comment-level quote-char)
	    (charmap-per-line (bp last-bp (if (or fixup-p
						  (and (eq state 'name)
						       ( paren-level 0)
						       (= (1+ time) times)))
					      (copy-bp last-bp)
					      nil))
			      ;; Per-line forms
			      ;; If at start of line and inside some parens,
			      ;; skip over some lines using memoized C-PARSE-LINE info.
			      ;; This is an invisible speed-up for the rest of this loop.
			      (#+later (when (and (zerop *first-index*) (> paren-level 0))
					 (multiple-value (line quote-char paren-level)
					   (c-forward-list-aux line
							       (and (eq state 'quote) quote-char)
							       paren-level
							       *last-line*))
					 (setq state (if quote-char 'in-quote 'in-space)
					       *this-is-the-last-line* (eq line *last-line*))))
              restart
	      (let* ((char (charmap-char))
		     (syntax (aref *c-syntax* (char-code char))))
		(when *debug-forward-cob*
		  (format t "<~A:~C:~A>" state char syntax))
		(selectq state
		  (in-name
		   (selectq syntax
		     (name)
		     (t
                      (and ( paren-level 0)
			   ( (incf time) times)
			   (charmap-return (charmap-bp-before)))
                      (setq state 'in-space)
		      (go restart))))
		  (in-quote
		   (cond ((char= char quote-char)
			  (and ( paren-level 0)
			       ( (incf time) times)
			       (charmap-return (charmap-bp-after)))
			  (setq state 'in-space))
			 ((char= char #/\)
			  (charmap-increment (if fixup-p (copy-bp last-bp) nil)))))
		  (in-comment
		   (cond ((and (send *interval* :get :comments-nest)
			       (char= #// char)
			       (if (char= #/* (progn (charmap-increment (if fixup-p (copy-bp last-bp) nil))
						 (charmap-char)))
				   (incf comment-level)
				 (go restart))))
			 ((and (char= #/* char)
			       (if (char= #// (progn (charmap-increment (if fixup-p (copy-bp last-bp) nil))
						 (charmap-char)))
				   (when ( (decf comment-level) 0)
				     (setq state 'in-space))
				 (go restart)))) ))
		  (in-space
		   (selectq syntax
		     (name
		      (setq state 'in-name))
		     (space)
		     (token
		      (and ( paren-level 0)
			   ( (incf time) times)
			   (charmap-return (charmap-bp-after))))
		     (quote
		      (setq state 'in-quote
			    quote-char char))
		     (open
		      (incf paren-level))
		     (close
		      (decf paren-level)
		      (and no-up-p
			   (< paren-level 0)
			   (charmap-return nil))
		      (and ( paren-level 0)
			   ( (incf time) times)
			   (charmap-return (charmap-bp-after)))
                      (setq state 'in-space))
		     (atsign
		      ;; not sure this works right with fixup-p if you run into the end???
		      (let  ((bp (forward-sexp (charmap-bp-before) 1 t 0 *to-bp* t t)))
			(setq line (bp-line bp)
			      index (bp-index bp)
			      *THIS-IS-THE-LAST-LINE* (EQ LINE *LAST-LINE*)
			      *LAST-INDEX* (IF *THIS-IS-THE-LAST-LINE*
					       (1- (BP-INDEX *TO-BP*))
					     (LINE-LENGTH LINE)))))
		     (t
		      (unless (listp syntax) (barf "Character ~C with bogus C syntax ~S encountered." char syntax))
		      (dolist (token syntax)
			(when (cl:string= line token :start1 index :end1 (+ index (string-length token)))
			  (dotimes (c (1- (string-length token)))
			    (charmap-increment))
			  (cond ((cl:string= token "//*")
				 (setq state 'in-comment
				       comment-level 0))
				((cl:string= token "*//")
				 (charmap-return (charmap-bp-after)))	;just halt at toplevel "*/"
				(t
				 (setq state 'in-space)
				 (and ( paren-level 0)
				      ( (incf time) times)
				      (charmap-return (charmap-bp-after)))))))))))))))
         (t
	  (let ((state 'in-space)	;STATE is one of IN-{ NAME QUOTE OPEN CLOSE SPACE ATSIGN TOKEN }
		(time 0)
		(first-bp (or stop-bp (interval-first-bp *interval*)))
		comment-level quote-char)
	    (rcharmap-per-line (bp first-bp (if (or fixup-p
						  (and (eq state 'name)
						       ( paren-level 0)
						       (= (1- time) times)))
					      (copy-bp first-bp)
					      nil))
			      ;; Per-line forms
			      ;; If at start of line and inside some parens,
			      ;; skip over some lines using memoized C-PARSE-LINE info.
			      ;; This is an invisible speed-up for the rest of this loop.
			      (#+later??? (when (and (zerop *first-index*) (> paren-level 0))
					 (multiple-value (line quote-char paren-level)
					   (c-forward-list-aux line
							       (and (eq state 'quote) quote-char)
							       paren-level
							       *last-line*))
					 (setq state (if quote-char 'in-quote 'in-space)
					       *this-is-the-last-line* (eq line *last-line*))))
              restart
	      (let* ((char (rcharmap-char))
		     (syntax (aref *reverse-c-syntax* (char-code char))))
		(when *debug-forward-cob*
		  (format t "<~A:~C:~A>" state char syntax))
		(selectq state
		  (in-name
		   (selectq syntax
		     (name)
		     (t
                      (and ( paren-level 0)
			   ( (decf time) times)
			   (rcharmap-return (rcharmap-bp-after)))
                      (setq state 'in-space)
		      (go restart))))
		  (in-quote
		   #+later??? (must deal with \ before anything else when running backwards)
		   (cond ((char= char quote-char)
			  (and ( paren-level 0)
			       ( (decf time) times)
			       (rcharmap-return (rcharmap-bp-before)))
			  (setq state 'in-space))
			 (nil
			  (rcharmap-decrement (if fixup-p (copy-bp first-bp) nil)))))
		  (in-comment
		   #+later (scan forward from toplevel if comments don't nest)
		   (cond ((and (send *interval* :get :comments-nest)
			       (char= #// char)
			       (if (char= #/* (progn (rcharmap-decrement (if fixup-p (copy-bp first-bp) nil))
						 (rcharmap-char)))
				   (incf comment-level)
				 (go restart))))
			 ((and (char= #/* char)
			       (if (char= #// (progn (rcharmap-decrement (if fixup-p (copy-bp first-bp) nil))
						 (rcharmap-char)))
				   (when ( (decf comment-level) 0)
				     (setq state 'in-space))
				 (go restart))))))
		  (in-space
		   (selectq syntax
		     (name
		      (setq state 'in-name))
		     (space)
		     (token
		      (and ( paren-level 0)
			   ( (decf time) times)
			   (rcharmap-return (rcharmap-bp-before))))
		     (quote
		      (setq state 'in-quote
			    quote-char char))
		     (close
		      (incf paren-level))
		     (open
		      (decf paren-level)
		      (and no-up-p
			   (< paren-level 0)
			   (rcharmap-return nil))
		      (and ( paren-level 0)
			   ( (decf time) times)
			   (rcharmap-return (rcharmap-bp-before)))
                      (setq state 'in-space))
		     (atsign)			; by the time we see this backwards we may have already lost
		     (t
		      (unless (listp syntax) (barf "Character ~C with bogus C syntax ~S encountered." char syntax))
		      (when *debug-forward-cob* (format t "{(~A0)~A-1~A}" paren-level time times))
		      ;; this is where c-m-B tries to back up over a multicharacter symbol.
		      (dolist (token syntax)
			(when *debug-forward-cob*
			  (format t "[~A]" (substring line (1+ (- index (string-length token))) (1+ index))))
			(when (let ((start (1+ (- index (string-length token)))))
				(and
				  ;; *** port this bug fix back to the TI ***
				  (not (minusp start))
				  (cl:string= line token :start1 start :end1 (1+ index))))
			  (dotimes (c (1- (string-length token)))
			    (rcharmap-decrement))
			  (cond ((cl:string= token "*//")
				 (setq state 'in-comment
				       comment-level 0))
				((cl:string= token "//*")
				 (rcharmap-return (rcharmap-bp-before)))	;just halt at toplevel "/*"
				(t
				 (setq state 'in-space)
				 (and ( paren-level 0)
				      ( (decf time) times)
				      (rcharmap-return (rcharmap-bp-before)))))))))))))))))

;;; my lmiti version defines useless stubs for forward-cob-aux, backward-cob-aux,
;;; c-find-comment-start, lisp-bp-syntactic-context, *c-parse-preparsed-flag*,
;;; c-parse-from-top, c-parse-line-memoized, c-parse-line,
;;; but I see no reason to do this now.

;;; fake code will have to do until I make write something that works for C:

(defun c-bp-syntactic-context (bp &optional (start-bp (forward-definition bp -1 t)))
  "Describe the syntactic context of a spot identified by BP.
The first value is non-NIL if that spot is in a string.
The second is non-NIL if that spot is in a multicharacter token like ==
	in which case it is the distance in characters to the start of the token.
The third is non-NIL if that spot is in a comment.
START-BP is where to parse from to compute these
 (default is start of containing defun)."
  (declare (return-list in-string in-token in-comment)
	   (ignore start-bp))
  (values nil nil nil))

;========================================================================

(defun make-c-syntax-array ()
  (declare (special *c-multicharacter-tokens*))
  (setq *c-multicharacter-tokens* nil)
  (let ((table (make-array 256.)))
    (dolist (syntax '((name #/A #/Z) (name #/a #/z) (name #/_) (name #/0 #/9) (name #/$)
		      (quote #/") (quote #/')
		      (open #/() (open #/[) (open #/{)
		      (close #/)) (close #/]) (close #/})
		      (space #/space) (space #/tab) (space #/line) (space #/page) (space #/return)
		      (atsign #/@)
		      ;;(escape #/\)
		      ! != /# % %= & &= &&
		      * *= *// + += ++ /, - -= -> -- // //* /. //=
		      /: /; < <= << <<= = == > >= >> >>= ?
		      ^ ^= /\ /| /|= /|/| ~))
      (if (atom syntax)
	  ;; handle a token in a string
	  (let* ((string (cl:symbol-name syntax))
		 (char (char-code (aref string 0))))
	    (if (and (= (string-length string) 1)
		     (null (aref table char)))
		;; single character token entry is 'token
		(setf (aref table char) 'token)
	      ;; multi character token entry is list of possible strings
	      (when (eq (aref table char) 'token)
		;; might have to convert from 'token to '("c")
		(setf (aref table char) (list (string (code-char char)))))
	      ;; make sure the resulting list is longest first
	      (setf (aref table char)
		    (cl:sort (cons string (aref table char)) '> :key 'string-length))))
	;; handle a special token in a list
	(let* ((symbol (first syntax))
	       (lo (second syntax))
	       (hi (if (cddr syntax) (third syntax) lo)))
	  (do ((char lo (code-char (1+ (char-code char)))))
	      ((char> char hi))
	    (setf (aref table (char-code char)) symbol)))))
    table))

(defun reverse-c-syntax-array (head)
  (let ((tail (cl:substitute 'token nil head :test-not #'(lambda (ignore item)
							(atom item))))
	(multicharacter-tokens
	  (cl:reduce #'(lambda (list item)
		      (if (atom item)
			  list
			(nconc list
			       (cl:remove
				 nil
				 item
				 :test #'(lambda (ignore string)
					   (= (string-length string) 1))))))
		  head
		  :initial-value nil)))
    (dolist (string multicharacter-tokens)
      ;; multi character token entry is list of possible strings
      (let ((char (char-code (aref string (1- (string-length string))))))
	(when (eq (aref tail char) 'token)
	  ;; might have to convert from 'token to '("c")
	  (setf (aref tail char) (list (string (code-char char)))))
	;; make sure the resulting list is longest first
	(setf (aref tail char)
	      (cl:sort (cons string (aref tail char)) '> :key 'string-length))))
    tail))

(defconst *c-syntax* (make-c-syntax-array) "C syntax table for zwei:forward-cob")
(defconst *reverse-c-syntax* (reverse-c-syntax-array *c-syntax*) "*c-syntax* with multicharacter tokens in reverse")

; End of ZCZWEI.LISP
