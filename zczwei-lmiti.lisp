; -*- mode: Lisp; Package: Zwei; Base: 10 -*-
; File: ZCZWEI.LISP
;
;	This code has been placed in the public domain.
;
; This file contains C mode for ZMACS, for MIT system 98, TI Rel 2, LMI Sys 102.


(defvariable *C-block-indentation* nil :fixnum-or-nil
  "The distance, in spaces, to indent nested C blocks.  If NIL, defaults to the
   current tab width.")

(defvariable *C-indent-}-as-outside* t :boolean
  "If T, a } is lined up with the statements *outside* the block it closes; if NIL,
   with the *inside* of the block.")

(defprop *flash-matching-paren* t mode-settable-p)

; System 102 defines these as stubs.  How useless!  Suppress redefinition warnings.
#+LMI (remprop 'com-c-mode ':source-file-name)
#+LMI (remprop 'c-mode-hook ':source-file-name)

; New name in LMI 102, may as well use it in other systems.
#-LMI (defmacro zwei-search (&rest etc) `(search . ,etc))

(defmajor com-c-mode c-mode "C"
	  "Set things up for editing C programs.
Makes comment delimiters //* and *//, Tab is Indent-for-C,
and anything else I think of." ()
  (set-char-syntax word-alphabetic *mode-word-syntax-table* #/_)
  (set-comtab *mode-comtab*
	      '(#\Tab com-Indent-for-C		; #\Line calls this automatically!
		#\c-m-Q com-Indent-Region-for-C	; in lieu of -Block-
		#\c-m-A com-Beginning-of-C-Function-or-Declaration
		#\c-m-E com-End-of-C-Function-or-Declaration
		#\c-m-H com-Mark-C-Function-or-Declaration
		#\c-m-R com-Reposition-Window-for-C
		;; Hyper-control and control-shift aren't synonymous
		;; in LMI system 102, TI Rel 2.
		#\h-c-C com-C-Compile-Region
		#\c-sh-C com-C-Compile-Region
		#\h-m-C com-C-Compile-Buffer-Changed-Sections
		#\m-sh-C com-C-Compile-Buffer-Changed-Sections
		#\m-/; com-End-Comment
		#\Break com-Break-for-C
		#\f4 com-Break-Lisp-in-C
		#\h-tab COM-C-LAST-OPEN-PAREN	;?? for debug only
		#\s-tab COM-JHB-INDENT		;?? for debug only
		#\f1 COM-PARSE-FROM-DEFUN 
		)
	      '(("C Compile Buffer" . com-C-compile-buffer)
		("C Syntax Check Region" . com-C-syntax-check-region)
		("C Syntax Check Buffer" . com-C-syntax-check-buffer)))
  (setq *space-indent-flag* t)
  (setq *paragraph-delimiter-list* nil)
  (setq *comment-start* "//*")
  (setq *comment-begin* "//*")
  (setq *comment-end* "*//")
  (setq *flash-matching-paren* nil)		; tries to parse Lisp!
  ;; Try to make at least some of the existing Lisp parsing work.
  ;; This appears to be sufficient to get matching brackets and braces to
  ;; work correctly, most of the time anyway.
  ;; There may be full-blown C parsing someday...
;  (aset list-alphabetic   *mode-list-syntax-table* #/_)     we do not want this.
  (aset list-alphabetic   *mode-list-syntax-table* #//)
  (aset list-alphabetic   *mode-list-syntax-table* #/|)	;??
  (aset list-slash        *mode-list-syntax-table* #/\)	;??
  (aset list-double-quote *mode-list-syntax-table* #/')
  (aset list-delimiter    *mode-list-syntax-table* #/,)
  (aset list-delimiter    *mode-list-syntax-table* #/;)
  (aset list-open         *mode-list-syntax-table* #/[)
  (aset list-close        *mode-list-syntax-table* #/])
  (aset list-open         *mode-list-syntax-table* #/{)
  (aset list-close        *mode-list-syntax-table* #/}))

(defprop c-mode :c editing-type)

(set-comtab *standard-comtab*
	    '()
	    '(("C Mode" . com-C-mode)))

(let ((pair '(:c . :c)))
  (unless (memq pair fs:*file-type-mode-alist*)
    (push pair fs:*file-type-mode-alist*)
    (push '(:h . :c) fs:*file-type-mode-alist*)))


; ================================================================
; Some utility functions.
; These were taken from ZCPARSE.LISP so that one could at least invoke C mode
; without having the rest of ZETA-C loaded.

(defun c-symbol-start-char-p (c)
  (or (and (>= c #/A) (<= c #/Z))
      (and (>= c #/a) (<= c #/z))
      (= c #/_) (= c #/$)))

(defun c-symbol-char-p (c)
  (or (c-symbol-start-char-p c)
      (and (>= c #/0) (<= c #/9))))

(defun c-symbol-from-string (string &optional (start 0) (end nil))
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
	    (values (c-intern-symbol (nsubstring string start index))
		    index)))))

; This started as a copy of zeta-c:zclex>intern.
(defun c-intern-symbol (name &rest options)
  "Interns a symbol in the current package.  Meaningful OPTIONS include :UPCASE,
   which causes conversion to upper case, and :SOFT, which causes the symbol not
   to be interned if not present.  '$' is interpreted as the package 
   prefix delimiter."
  (when (memq ':upcase options)
    (dotimes (i (string-length name))
      (aset (char-upcase (aref name i)) name i)))
  (nlet (($pos (string-search-char #/$ name))
	 ((pkg
	    (and $pos (pkg-find-package (string-upcase (nsubstring name 0 $pos))
					:find *package*)))
	  ((pkg name (if (and pkg (neq pkg *package*))
			 (values pkg (nsubstring name (1+ $pos)))
			 (values *package* name))))))
    (if (memq :soft options)
	(intern-soft name pkg)
	(intern name pkg))))


; ================================================================
; Sectionization.

(defun (:c section-p) (line)
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

(defun (:c get-section-name) (line ignore)
  "Gets the name of the section starting on LINE.  Returns three values: the symbol
   or function spec defined, or NIL; the string which appears in the line to
   specify that symbol or spec; and T if the line doesn't begin a section after
   all."
  (declare (values symbol string error-p))
  (if (not (c-section-p line nil))
      (values nil nil t)
    (do ((sline line)
	 (sidx 0 (1+ sidx))
	 (parendepth 0)
	 possible-func-name
	 probable-func-name)
	(nil)
      (or sline
	  (ferror "Sectionization failure on line: ~%~%~A~%~@
		     Lines that start in the middle of comments must be indented at least~@
		     one space; if this is such a line, indent it to prevent this error.~@
		     Otherwise, you have found a bug in the ZETA-C sectionizer; please~@
		     report it, including several lines of the text of your file above~@
		     and below where the error occurred." line))
      (cond ((>= sidx (string-length sline))
	     (if (and probable-func-name (zerop parendepth))
		 (progn (setf (get probable-func-name 'function-section-p) t)
			(return probable-func-name (string probable-func-name) nil))
	       (setq sline (line-next sline))
	       (setq sidx -1)))
	    ((%string-equal sline sidx "//*" 0 2)
	     (nlet ((next-bp gave-up
			     (zwei-search (create-bp sline sidx) "*//" nil nil 2)))
	       (if gave-up
		   ;; If couldn't find end of comment in 2 lines, give up
		   ;; (?? is this a reasonable strategy?)
		   (setq sidx (string-length sline))
		 (setq sline (bp-line next-bp))
		 (setq sidx (1- (bp-index next-bp))))))
	    (t
	     (let ((c (ldb %%ch-char (aref sline sidx))))
	       (cond ((= #/( c)
		      (incf parendepth)
		      (setq probable-func-name possible-func-name))
		     ((= #/) c)
		      (decf parendepth))
		     ((= #/: c)		; Sigh, it's a label in column 0.
		      (return (values nil nil t)))
		     ((and (= #/{ c)
			   probable-func-name
			   (zerop parendepth))
		      (setf (get probable-func-name 'function-section-p) t)
		      (return probable-func-name (string probable-func-name) nil))
		     ((and (= #/{ c)	; check for struct/union tag.
			   (zerop parendepth)
			   (not (memq possible-func-name 'c:(|struct| |union|))))
		      (return possible-func-name (string possible-func-name) nil))
		     ((and (or (= #/, c) (= #/; c) (= #/= c) (= #/[ c))
			   (zerop parendepth))
		      (let ((sec-name (or probable-func-name possible-func-name
					  (format nil "~A-declaration-~D"
						  (line-buffer-pathname-name line)
						  (incf *section-count*)))))
			(return sec-name (string sec-name) nil)))
		     ((c-symbol-start-char-p c)
		      (nlet ((sym next (c-symbol-from-string sline sidx)))
			(setq sidx (1- next))
			(setq possible-func-name sym))))))))))

; Extracted from (:lisp get-section-name) in SYS: ZWEI; SECTIO.
(defun line-buffer-pathname-name (line)
  "Given a line, finds a string appropriate for naming the buffer the line appears
   in."
  (let ((buffer (node-top-level-node (line-node line))))
    (if (buffer-pathname buffer)
	(let ((name
		(pathname-name (buffer-pathname buffer))))
	  (if (consp name)
	      (apply 'string-append
		     (mapcar #'(lambda (name-elt)
				 (if (consp name-elt)
				     (car name-elt) name-elt))
			     name))
	      (string name)))
	(buffer-name buffer))))


; ================================================================
; Command support routines.

(defun forward-c-object (bp &optional (times 1) fixup-p)
  "Return a bp which is forward across TIMES top-level C objects (declarations or
   function definitions) from BP.  If BP is within such an object, that is included
   in the count.  TIMES negative means move backwards.  FIXUP-P non-NIL means if we
   attempt to move over the beginning or end of the buffer, return a bp to there;
   otherwise return NIL.
/Note this is intentionally /"stupid/": it does no brace counting or the like but
   just goes on the simplest textual cues, so it will work (assuming the
   conventions are followed) even when semicolons and braces are missing."
  (if (zerop times)
      (copy-bp bp)
      (if (plusp times)
	  (or (forward-c-object-forward bp times)
	      (and fixup-p (copy-bp (interval-last-bp *interval*))))
	  (or (forward-c-object-backward bp (- times))
	      (and fixup-p (copy-bp (interval-first-bp *interval*)))))))

; Internal to forward-c-object.
(defun forward-c-object-forward (bp times)
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

; Internal to forward-c-object.
(defun forward-c-object-backward (bp times)
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
  "Skips backward to the beginning of the line just after the last non-blank,
   non-comment line preceding BP.  (BP is assumed not to be in a comment.)  A
   comment beginning on a line with other stuff on it is not skipped over.  If
   COMMENTS-ONLY, only goes back to the beginning of the first comment before BP.
   A line containing a preprocessor command counts as a blank line."
  (let ((prev-thing (backward-over *whitespace-chars* bp)))
    (or (and (bp-= prev-thing (interval-first-bp *interval*))
	     prev-thing)
	(and (looking-at-backward prev-thing "*//")
	     (nlet ((comment-start (or (zwei-search prev-thing "//*" t)
				       (barf "Can't find beginning of comment.")))
		    ((prev-thing-on-line (backward-over '(#\Space #\Tab)
							comment-start))))
	       (and (zerop (bp-index prev-thing-on-line))
		    (c-skip-backward-blank-and-comment-lines prev-thing-on-line
							     comments-only))))
; Then again, maybe preprocessor commands shouldn't count.  (The bug is that only
; one of a matched pair, #if ... #endif or #lisp ... #endlisp, gets picked up.)
;	   (and (not comments-only)
;		   (let ((beg-line-bp (beg-line prev-thing 0 t)))
;			(and (looking-at beg-line-bp "#")
;				(c-skip-backward-blank-and-comment-lines beg-line-bp))))
	(if comments-only bp
	    (beg-line prev-thing 1 t)))))

; Similar to DEFUN-INTERVAL (SYS: ZWEI; FOR), q.v.
(defun c-object-interval (bp &optional (times 1) fixup-p (comments-p t)
			  (top-blank-p nil))
  "Return an interval surrounding the top-level C object that BP is within, or NIL.
  If TIMES is > 1, includes additional objects after that one.  COMMENTS-P non-NIL
  means include comments before the object.  TOP-BLANK-P non-NIL along with
  COMMENTS-P means include one blank line (if any) before anything else.
  The second value is the first line of the object proper (comments etc. not
  included)."
  (declare (values interval definition-line))
  (nlet ((this-obj-end (forward-c-object bp 1 t))
	 ((obj-start (forward-c-object this-obj-end -1 fixup-p)))
	 ((end (forward-c-object this-obj-end (1- times) fixup-p))))
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
(defun indent-interval-for-C (bp1 &optional bp2 in-order-p point-line
			      (comments-p t))
  "Indent all the lines in the specified interval for C.  A line is in the interval
   iff its beginning is included.  If COMMENTS-P is NIL, comments are not
   readjusted.  Returns a BP to the end of the interval adjusted.  Normally, blank
   lines are left with no indentation; however, if POINT-LINE is supplied, that
   line is indented even if blank (as the name suggests, this is typically the
   preferred treatment for the line containing point)."
  (get-interval bp1 bp2 in-order-p)
  (interval-lines (bp1 bp2) (start-line stop-line)
    (do ((line start-line (line-next line))
	 (tbp (create-bp start-line 0))
	 (indent-increment (if *C-block-indentation*
			       (* *C-block-indentation* (font-space-width))
			       (send (window-sheet *window*) ':editor-tab-width)))
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
	  (nlet ((prev-non-blank-line (line-previous-non-blank-or-comment line t))
		 ((prev-line-ind (line-indentation prev-non-blank-line))
		  (last-thing-bp (end-of-real-text prev-non-blank-line))
		  (unmatched-open (c-last-open-paren prev-non-blank-line))
		  ((goal-ind (if (not (and *C-indent-}-as-outside*
					   (looking-at tbp "}")))
				 prev-line-ind
				 (max 0 (- prev-line-ind indent-increment)))))))
	    (cond (unmatched-open
		   (indent-line tbp (+ (BP-INDENTATION unmatched-open) (font-space-width))))
		  ((looking-at-backward last-thing-bp "{")
		   (indent-line tbp (+ goal-ind indent-increment)))
		  ((and (not *C-indent-}-as-outside*)
			(looking-at-backward last-thing-bp "}"))
		   (indent-line tbp (max 0 (- goal-ind indent-increment))))
		  (t (indent-line tbp goal-ind)))
	    (when comments-p (indent-for-comment tbp)))))))

(defun line-previous-non-blank-or-comment (line &optional fixup-p)
  "Returns the first non-blank line before LINE which does not begin with a
   comment (or #).  FIXUP-P means, if we reach the first line of the buffer
   and it's blank, return it anyway.
   If we are already on the first line of the buffer, return NIL."
  (do ((line (line-previous line) (line-previous line)))
      ((or (null line)
	   (let ((first-thing (forward-over *blanks* (create-bp line 0))))
	     (and (not (line-blank-p line))
		  (not (looking-at first-thing *comment-start*))
		  (not (looking-at first-thing "#"))))
	   (and fixup-p (null (line-previous line))))
       line)))

(defun end-of-real-text (line)
  "Returns a BP that points right after the last non-whitespace, non-comment
   character on LINE."
  ;;Need to adjust this so we can have more than one comment on a line. ??
  (nlet ((comment-start ignore inside-string
			(c-find-next-comment-start line)))
    (backward-over *blanks*
		   (if (and comment-start (not inside-string))
		       (create-bp line comment-start)
		       (create-bp line (string-length line))))))

; Similar to COMPILE-DEFUN-INTERNAL (SYS: ZWEI; COMC), q.v.
(defun c-compile-object-internal (compile-p mode-name echo-name
				  &optional use-typeout)
  "C-compile a part of the current buffer.  If COMPILE-P is NIL, the object is only
   parsed (for syntax checking).  MODE-NAME is a string containing a capitalized
   present participle, such as /"Compiling/"; ECHO-NAME is a string containing a
   lowercase past participle and period (/"compiled./").  USE-TYPEOUT is passed to
   C-COMPILE-PRINT-INTERVAL and controls where information is printed."
  (nlet ((bp1 bp2 object-name
	      (if (window-mark-p *window*)
		  (values (point) (mark) "region")
		  (let ((int (or (c-object-interval (point) 1 nil nil)
				 (barf "Don't see what to compile."))))
		    (values (interval-first-bp int) (interval-last-bp int) nil)))))
    (c-compile-print-interval bp1 bp2 nil compile-p object-name
			      mode-name echo-name use-typeout)))

; Similar to COMPILE-BUFFER (SYS: ZWEI; COMC), q.v.
(defun c-compile-buffer (compile-p mode-name echo-name rest-of-buffer)
  "C-compile or syntax check the current buffer.  If COMPILE-P is NIL, the buffer
   is only parsed (for syntax checking).  MODE-NAME is a string containing a
   capitalized present participle, such as /"Compiling/"; ECHO-NAME is a string
   containing a lowercase past participle and period (/"compiled./").  If
   REST-OF-BUFFER, starts compiling at point."
  (nlet ((bp1 name (if rest-of-buffer (values (point) "rest of buffer")
		       (values (interval-first-bp *interval*) "buffer")))
	 (bp2 (interval-last-bp *interval*)))
    (c-compile-print-interval bp1 bp2 t compile-p name mode-name echo-name nil)))

; Similar to COMPILE-PRINT-INTERVAL (SYS: ZWEI; COMC), q.v.
(defun c-compile-print-interval (bp1 bp2 in-order-p compile-p object-name mode-name
				 echo-name use-typeout
				 &optional already-sectionized)
  "C-compile or parse (syntax check) the interval specified by BP1, BP2,
   IN-ORDER-P.  COMPILE-P is NIL for syntax checking only.  OBJECT-NAME is a string
   to print as the name of this whole object, or NIL to mention each object's name.
   USE-TYPEOUT can be T (print in typeout window), NIL (print in echo area), or
   :PROMPT (print in prompt line).  MODE-NAME is a string containing a capitalized
   present participle, such as /"Compiling/"; ECHO-NAME is a string containing a
   lowercase past participle and period (/"compiled./")."
  (get-interval bp1 bp2 in-order-p)
  (when (not already-sectionized)
    (check-interval-sections bp1 bp2 t))
  (undo-save-current-range)
  (let ((format-function
	  (selectq use-typeout
	    ((t) #'(lambda (string &rest args)
		     (lexpr-funcall #'format t string args)))
	    (:prompt #'prompt-line-more)
	    ((nil) #'(lambda (string &rest args)
		       (lexpr-funcall #'format query-io string args)))))
	(success))
    (if object-name
	(funcall format-function "~&~A ~A" mode-name object-name)
	(funcall format-function "~&~A ~S" mode-name
		 (section-node-name (bp-node bp1))))
    (unwind-protect
	(progn
	  (c-compile-interval compile-p (if (eq use-typeout 't) t query-io)
			      bp1 bp2 t)
	  (setq success t))
      (or success (funcall format-function " -- aborted.")))
    (funcall format-function " -- ~A" echo-name)
    (update-interval-compile-tick bp1 bp2 t)))

; Similar to COMPILE-INTERVAL (SYS: ZWEI; COMC), q.v.
; Saving the location of the first read-error in a "q-register" is gross, but this
; is the way the LMITI system works, and (for the moment, at least) I might as well
; follow along.
(defun c-compile-interval (compile-p-arg print-results-stream bp1
			   &optional bp2 in-order-p)
  "C-compile or parse (syntax check) the interval specified by BP1, BP2,
   IN-ORDER-P.  Does not print any sort of message saying what is being compiled;
   does not know about sectionization.  COMPILE-P-ARG is NIL for syntax checking
   only."
  (declare (special print-results-stream))
  (get-interval bp1 bp2 in-order-p)
  (nlet ((generic-pathname (send *interval* :send-if-handles :generic-pathname))
	 (whole-file (and (bp-= bp1 (interval-first-bp *interval*))
			  (bp-= bp2 (interval-last-bp *interval*))))
	 (interval-stream (interval-stream bp1 bp2))
	 (start-line (count-lines (interval-first-bp *interval*) bp1 t))
	 ((stream (zeta-c:make-c-parser interval-stream generic-pathname
					:start-line start-line 
					:c-package package
					:whole-file whole-file)))
	 (attr-vars attr-vals (send *interval* ':attribute-bindings)))
    (remprop (make-register-name #/.) 'point)
    (let ((compile-processing-mode 'compiler:macro-compile)
	  (defvar-hack nil)
	  (compile-p (or compile-p-arg #'ignore)))
      (declare (special compile-processing-mode defvar-hack compile-p))
      (progv attr-vars attr-vals
	(when fs:this-is-a-patch-file
	  (putprop generic-pathname t ':patch-file))
	(flet ((do-it ()
		      (compiler:compile-stream stream generic-pathname nil
					       'compile-interval-process-fn
					       t nil package nil nil whole-file)))
	  (if compile-p
	      (compiler:locking-resources-no-qfasl (do-it))
	      (do-it)))))
    (and generic-pathname
	 (si:record-file-definitions generic-pathname si:fdefine-file-definitions
				     whole-file))))

; Similar to COMPILE-BUFFER-CHANGED-FUNCTIONS (SYS: ZWEI; COMC), q.v.
(defun c-compile-buffer-changed-functions (buffer ask-p compile-p names)
  "Re-C-compile or syntax-check all changed sections in BUFFER (that contain
   function definitions).  COMPILE-P is NIL for syntax-checking only.  ASK-P says
   whether to query user about each function to be compiled.  NAMES has three
   elements, like '(/"Compile/" /"Compiling/" /"compiled./")."
  (let ((*interval* buffer))
    (resectionize-buffer buffer)
    (mapc #'(lambda (node)
	      (and (typep node 'section-node)
		   (not (stringp (section-node-name node)))
		   (not (bp-= (interval-first-bp node) (interval-last-bp node)))
		   (> (node-tick node) (section-node-compile-tick node))
		   (or (not ask-p)
		       (fquery '(:select t) "~A ~A? " (car names)
			       (section-node-name node)))
		   (c-compile-print-interval node nil t compile-p nil
					     (cadr names) (caddr names) t)))
	  (node-inferiors *interval*))))


; ================================================================
; Commands.

(defcom com-beginning-of-c-function-or-declaration
	"Moves to the beginning of the current C function or declaration." (km)
  (let ((bp (or (forward-c-object (point) (- *numeric-arg*)) (barf))))
    (point-pdl-push (point) *window*)
    (move-bp (point) bp))
  dis-bps)

(defcom com-end-of-c-function-or-declaration
	"Moves to the end of the current C function or declaration." (km)
  (let ((bp (or (forward-c-object (point) *numeric-arg*) (barf))))
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
  (let ((end (indent-interval-for-C (beg-line (point))
				    (beg-line (point) *numeric-arg* t)
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

(defcom com-C-compile-region
	"C-compiles the current region or function//declaration.
If there is a region, it is compiled; otherwise, the current or next
function or declaration is compiled." ()
  (c-compile-object-internal t "C-compiling" "compiled.")
  dis-none)

(defcom com-C-syntax-check-region
	"Checks the current region or function//declaration for correct C syntax.
If there is a region, it is checked; otherwise, the current or next function or
declaration is checked." ()
  (c-compile-object-internal nil "C-syntax-checking" "syntax-checked.")
  dis-none)

(defcom com-C-compile-buffer
	"C-compiles the entire current buffer; or, with an argument, the rest of
/the buffer (starting at point)." ()
  (c-compile-buffer t "C-compiling" "compiled." *numeric-arg-p*)
  dis-none)

(defcom com-C-syntax-check-buffer
	"Checks the entire current buffer for correct C syntax; or, with an
argument, the rest of the buffer (starting at point)." ()
  (c-compile-buffer nil "C-syntax-checking" "syntax-checked." *numeric-arg-p*)
  dis-none)

(defcom com-C-compile-buffer-changed-sections
	"C-compile any sections in this buffer which have been edited.
A numeric arg means ask about each section individually." ()
  (si:file-operation-with-warnings ((and (buffer-file-id *interval*)
					 (send (send *interval* ':generic-pathname)
					       ':generic-pathname))
				    ':compile nil)
    (compiler:compiler-warnings-context-bind
      (c-compile-buffer-changed-functions
	*interval* *numeric-arg-p* t '("C-compile" "C-compiling" "compiled."))))
  (format t "~&Done.~%")
  dis-none)

; Similar to COM-REPOSITION-WINDOW in SYS:ZWEI;COME
; Should indirect through major mode!
(defcom com-Reposition-Window-for-C
	"Try to get all of current C function//declaration in the window.
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
        (int (c-object-interval (point) 1 t t))
        start-bp end-bp
	recenter-bp)
    (cond ((not (null int))
	   (setq start-bp (interval-first-bp int)
		 end-bp (interval-last-bp int))
	   ;; Don't include the blank line after the defun
	   (and (zerop (bp-index end-bp)) (setq end-bp (end-line end-bp -1 t)))
	   (cond ((and (pline-of-point t *window* start-bp)	;If start is on screen
		       (null (pline-of-point t *window* end-bp))	; but not end
		       (multiple-value-bind (line index)
			   (put-point-at-pline sheet (bp-line end-bp)
					       (bp-index end-bp) (1- n-plines)
					       (interval-first-bp *interval*)
					       (interval-last-bp *interval*))
			 (setq recenter-bp (create-bp line index))
			 ;; And can fit bottom of the defun on as well
			 ;; then start at the top of the function.
			 (not (bp-< start-bp recenter-bp)))))
		 ((bp-< start-bp
			(setq recenter-bp (multiple-value-bind (line index)
					      (put-point-at-pline
						sheet (bp-line point)
						(bp-index point) (1- n-plines)
						(interval-first-bp *interval*)
						(interval-last-bp *interval*))
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
					  (interval-last-bp *interval*))
		    (setq recenter-bp (create-bp line index))))
		 (t
		  ;; If already at default place, exclude comments above defun.
		  (and (bp-= (window-start-bp *window*) start-bp)
		       (setq start-bp
			     (interval-first-bp
			       (c-object-interval (point) 1 t nil))))
		  (setq recenter-bp start-bp)))
	   (recenter-window *window* ':start recenter-bp))
	  (t (barf "Can't find a C function or declaration here.")))
    dis-none))

(defcom com-Break-for-C "Invokes a C listener on the typeout window." ()
  (unwind-protect
      (let ((*inside-break* t))
	;; *inside-break* talks to (:method editor-typeout-window :more-exception).
	(zeta-c:c-top-level *typeout-window* t
			    (if (typep *interval* 'file-buffer)
				(send *interval* ':pathname)
				(fs:default-pathname)))))
  (send *typeout-window* ':make-complete)
  dis-none)


; End of old ZCZWEI.LISP

;;@@@This is stuff from the now-defunct JHBMODE, to be slipped in above and used as time goes on.
;; The thing to do is make this file work for all A-machines, with conditional compilation.
;; Sadly, ZWEI on the the Symbolics machines is done incompatibly (with more methods instead
;; of traditional function calls), and so must be maintained separately.

(DEFGLOBAL *C-PARSE-PREPARSED-FLAG* NIL		;We may not need this, so be prepared to delete it.
  "If this is T, C-PARSE-FROM-DEFUN assumes that the lines are already parsed.")

(defcom com-Break-Lisp-in-C "Invokes a Lisp listener on the typeout window. To be changed." ()
  (UNWIND-PROTECT
      (LET ((*INSIDE-BREAK* T))
	(BREAK "ZMACS")))
  (FUNCALL STANDARD-OUTPUT ':MAKE-COMPLETE)
  DIS-NONE)

(defun jhbshobuf (&optional (firstline (bp-line (interval-first-bp *interval*))))
  "Used to examine the LINE-CONTENTS-PLIST of all lines from FIRSTLINE onward."
  (do ((x firstline (line-next x)))
      ((not x) :done)
    (format t "~%~A   ~A" (line-contents-plist x) x)))

(defcom COM-C-LAST-OPEN-PAREN "Move POINT to the last valid, unmatched open-parenthesis, if it exists." ()
  (let* ((line (bp-line (point)))
	 (dest-bp (c-last-open-paren line)))
    (when dest-bp (move-bp (point) dest-bp)))
  dis-text)


(defcom COM-JHB-INDENT "My first try at the important issue of indentation." ()
  ;;Eventually we want to parse-from-defun first, then check to see if this line is in a string??
  ;; Be sure to go over the LISP-indent and make ours feels exactly the same??
  (unless nil					;Check if we are in a string?? if so do nil
    (let ((end (jhb-indent-interval (beg-line (point))
				    (beg-line (point) *numeric-arg* t)
				    nil (bp-line (point)) *numeric-arg-p*)))
      (if *numeric-arg-p*
	  (move-bp (point) end)
	  (indent-bp-adjustment (point)))))
  dis-text)

(defcom COM-PARSE-FROM-DEFUN "Experimental" ()
  (C-PARSE-FROM-DEFUN (bp-line (point)))
  dis-text)

#||
"BP1 can be an interval, in which case bp2 should be nil. Indentation for C.  Returns a BP to the end of the indentation, usually the first printing char on the line. If COMMENTS-P is NIL, comments are not readjusted. If INDENT-BLANK-P is NIL, a blank line is left with no *BLANK* chars."	;replace below when done.
(defun JHB-INDENT-INTERVAL (bp1 &optional bp2 in-order-p point-line (comments-p t))
  (GET-INTERVAL BP1 BP2 IN-ORDER-P)
  (C-PARSE-FROM-DEFUN (BP-LINE BP2))
  (interval-lines (bp1 bp2) (start-line stop-line)
    (when (and (= (bp-index bp2) 0)		;Special case for blank line at EOB.
	       (null (line-next (bp-line bp2)))
	       (bp-= bp1 bp2))
      (setq stop-line nil))
    (do (model-line 
	 (line start-line (line-next line))
	 (tbp (create-bp start-line 0))
	 (*C-PARSE-PREPARSED-FLAG* T)
	 (indent-increment (if *C-block-indentation*
			       (* *C-block-indentation* (font-space-width))
			       (send (window-sheet *window*) :editor-tab-width))))
	((eq line stop-line)
	 (if line (move-bp tbp line 0) (interval-last-bp *interval*)))	;??Scrap this in favor of the Lisp mode way.
      (indent-bp-adjustment (move-bp tbp line 0))
      (if (or (null (line-previous line))	;Never indent:     (interval-first-bp ...),
	      (and (line-blank-p line) (neq line point-line))	;blanks, except cursor line,
	      (looking-at tbp "#"))		;or preprocessor commands.
	  (indent-line tbp 0)			;THEN snuggle up to that left margin,
	  ;; ELSE we have to do some real work...
	  
	  (setq model-line (line-previous-non-blank-or-comment line t))
	  (end-line model-line)
	  
	  ***
	  (do* ((BP (end-of-line (line-previous line)) (dbp BP))	;Start at end of prev-line.
		(char (and BP (ldb %%ch-char (bp-char BP)))	;Strip font, checking for valid BP first.
		      (and BP (ldb %%ch-char (bp-char BP))))
		
		(found-keyword (and (memq char '(#/f #/w #/i #/F #/W #/I)) ;;Efficiency- usually we 
				    (or (looking-at BP "FOR")
					(looking-at BP "WHILE")
					(looking-at BP "IF")))
			       (and (memq char '(#/f #/w #/i #/F #/W #/I)) ;;report failure right here.
				    (or (looking-at BP "FOR")
					(looking-at BP "WHILE")
					(looking-at BP "IF")))))
	       
	       ((or (eq char #/;)		;Quit upon reaching a semi-colon,
		    (eq char #/()		; open-paren,
		    (not BP)			; the beg of buffer,
		    found-keyword)		; or a keyword.
		(when found-keyword  BP))
	    
	    (when (= char #/))			;Skip over any complete parenthetical expression.
	      (ibp BP)				;This puts us in position for (forwrd-sexp ...)
	      (move-bp BP (forward-sexp BP -1)))
	    (c-skip-string BP nil char)		;Likewise any string or comment.
	    ))
      ***
      (let* ((model-line (line-previous-non-blank-or-comment line t))
	     
	     (model-pxl-indent (line-indentation model-line))
	     (last-thing-bp (end-of-real-text model-line))
	     (FWI (C-FWI-indent line))		;For, While or If.
	     (unmatched-open (c-last-open-paren model-line))
	     (goal-pxl-indent (if (and *C-indent-}-as-outside*
				       (looking-at tbp "}"))
				  (max 0 (- model-pxl-indent indent-increment))
				  model-pxl-indent)))
	(setq goal-pxl-indent
	      (cond (unmatched-open (+ (BP-INDENTATION unmatched-open) (font-space-width)))
		    (FWI (+ (BP-INDENTATION FWI) indent-increment))
		    ((looking-at-backward last-thing-bp "{")	;This will change to account for many
		     (+ goal-pxl-indent indent-increment))	;Push.
		    ((and (not *C-indent-}-as-outside*)
			  (looking-at-backward last-thing-bp "}"))
		     (max 0 (- goal-pxl-indent indent-increment)))	;Pop.
		    (t goal-pxl-indent)))
	(indent-line tbp goal-pxl-indent)	;Here is where the work actually gets done.
	(when comments-p (indent-for-comment tbp))))))	;Maybe write our own??

||#

(defun old-JHB-INDENT-INTERVAL (bp1 &optional bp2 in-order-p point-line (comments-p t))	;for reference only
  "Indent one line for C.  Returns a BP to the end of the indentation, 
usually the first printing char on the line.
If COMMENTS-P is NIL, comments are not readjusted.
If INDENT-BLANK-P is NIL, a blank line is left with no *BLANK* chars."
  ;;Strategy: An un-matched open-paren lying about comes first, and is final.
  ;;Next come those things which can push or pop us one or more levels: IF WHEN ELSE, {},
  ;;Last we just copy the indentation of the previous non-blank line.
  (or in-order-p (order-bps bp1 bp2))
  (interval-lines (bp1 bp2) (start-line stop-line)
    (when (and (= (bp-index bp2) 0)		;Special case for blank line at EOB.
	       (null (line-next (bp-line bp2)))
	       (bp-= bp1 bp2))
      (setq stop-line nil))
    (do ((line start-line (line-next line))
	 (tbp (create-bp start-line 0))
	 (indent-increment (if *C-block-indentation*
			       (* *C-block-indentation* (font-space-width))
			       (send (window-sheet *window*) :editor-tab-width))))
	((eq line stop-line)
	 (if line (move-bp tbp line 0) (interval-last-bp *interval*)))
      (indent-bp-adjustment (move-bp tbp line 0))
      (if (or (null (line-previous line))	;Never indent:   (interval-first-bp ...),
	      (and (line-blank-p line) (neq line point-line))	;blanks, except cursor line,
	      (looking-at tbp "#"))		;or preprocessor commands.
	  (indent-line tbp 0)			;THEN snuggle up to that left margin,
	  ;; ELSE we have to do some real work...
	  (let* ((model-line (line-previous-non-blank-or-comment line t))
		 (model-pxl-indent (line-indentation model-line))
		 (last-thing-bp (end-of-real-text model-line))
		 (FWI (C-FWI-indent line))
		 (unmatched-open (c-last-open-paren model-line))
		 (goal-pxl-indent (if (and *C-indent-}-as-outside*
					   (looking-at tbp "}"))	;??Ever stacked up??
				      (print (max 0 (- model-pxl-indent indent-increment)))
				      model-pxl-indent)))
	    (setq goal-pxl-indent
		  (cond (unmatched-open (print "UMO")
					(+ (BP-INDENTATION unmatched-open) (font-space-width)))
			
			(FWI (print "FWI")(+ (BP-INDENTATION FWI) indent-increment))
			
			((looking-at-backward last-thing-bp "{")  (print "{")	;Push.
								  (+ goal-pxl-indent indent-increment))	; Many #\{s?? YES.
			
			((and (not *C-indent-}-as-outside*)
			      (looking-at-backward last-thing-bp "}")) (print "}")
			 (max 0 (- goal-pxl-indent indent-increment)))	;Pop.
			
			(t goal-pxl-indent)))
	    (print  goal-pxl-indent)
	    (indent-line tbp goal-pxl-indent)
	    (when comments-p (indent-for-comment tbp)))))))

(defun c-last-open-paren (line)
  "Return either NIL or a bp which points to the last valid 
/(outside a string or comment), unmatched open-parenthesis in LINE."
  ;;Strategy: 
  ;;
  ;;index <- 0.
  ;;LOOP
  ;;find the first valid open after index (0, at first).
  ;;IF ~, quit, returning last-valid-one (the last valid open, or NIL, reporting failure).
  ;;   ELSE set index to (1+ pos).
  ;;find the matching close (not just any close).
  ;;IF , set index to (1+ pos), and go back to LOOP.
  ;;   ELSE last-valid-one <- index,  and go back to LOOP.
  
  (do (next-close
       last-valid-one
       (ind 0) 
       (next-open (C-sensitive-string-search #/( line 0)
		  (C-sensitive-string-search #/( line (1+ ind))))
      ((not next-open)
       (when last-valid-one
	 (create-bp line last-valid-one)))
    
    (setq ind next-open
	  next-close (c-matching-close-paren-in-line line next-open))
    
    (if next-close
	(setq ind next-close)
	(setq last-valid-one next-open))))



(defun c-matching-close-paren-in-line (line index)
  "Like FORWARD-SEXP.
In LINE, find the next open-paren after INDEX (normally right at INDEX),
and return either the index of its mate, or NIL if it has no mate on this line."
  ;;Recursion is a good idea here cos we stay in one line.
  ;;Maybe we could assume index points to a #\(, but it is easy to make sure.
  ;;Maybe we can get rid of ndx, and just hack INDEX.
  (setq index (C-sensitive-string-search #/( line index))
  (when index
    (do (open2 close (ndx index))
	((not ndx))				;If ndx shows up nil, we failed to find a match.
      
      (setq open2 (C-sensitive-string-search #/( line (1+ ndx)))
      (setq close (C-sensitive-string-search #/) line (1+ ndx)))
      
      (cond ((not close) (return nil))		;~ close, return NIL.
	    
	    ((not open2) (return close))	; close, ~ open, return close.
	    
	    (t					; close, and  open:
	     (if (< close open2)		;If the close comes 1st, return it
		 (return close)			;else skip over the () and keep looking.
		 (setq ndx (c-matching-close-paren-in-line line open2))))
	    ))))



(defun C-sensitive-string-search (KEY STRING &OPTIONAL (FROM 0) TO (KEY-FROM 0) KEY-TO
				  CONSIDER-CASE &AUX
				  (ALPHABETIC-CASE-AFFECTS-STRING-COMPARISON CONSIDER-CASE)
				  KEY-LEN)
  "Just like an ordinary STRING-SEARCH, but skips over 
strings and comments without searching their contents. 
Rules for nesting comments are found on the file's plist.
Obviously this function will not do the right thing if 
you ask it to find a string or comment delimiter."
  ;;Should we do the same for SEARCH?
  ;;??Make this sensitive to the previous line.
  ;;??Make this deal with slashified chars!
  ;;??Are there any other conds under which we might find quotes (either kind)?
  ;;Look for BACKslash?? Ubique.
  (si:COERCE-STRING-SEARCH-ARG STRING)
  (si:COERCE-STRING-ARG KEY)
  (UNLESS KEY-TO
    (SETQ KEY-TO (ARRAY-ACTIVE-LENGTH KEY)))	;NB-- Have I failed to do this elsewhere??
  (SETQ KEY-LEN (- KEY-TO KEY-FROM))
  (OR TO (SETQ TO (1+ (- (ARRAY-ACTIVE-LENGTH STRING)	;Last position at which key may start +1
			 KEY-LEN))))		;Key-Len will be  1.
  
  (COND ((= KEY-FROM KEY-TO)
	 (AND ( FROM TO) FROM))
	(T
	 (PROG (CH1)
	       (COND ((MINUSP TO) (RETURN NIL)))
	       (SETQ CH1 (AREF KEY KEY-FROM))
	    LOOP				;Find next place key might start
	       (OR (SETQ FROM
			 (do ((index FROM)
			      (next-ind (string-search-set (list #/' #/" #// CH1) STRING FROM)
					(string-search-set (list #/' #/" #// CH1) STRING index)))
			     
			     ;;When ~ any more interesting chars on this line, we failed to find it.
			     ((or (not next-ind) (> next-ind TO)))
			   (case (AREF STRING next-ind)
				 (#/" (setq index (1+ (or (%STRING-SEARCH-CHAR
							    #/" STRING (1+ next-ind) (1- TO))
							  (1- TO)))))
				 (#/' (setq index (1+ (or (%STRING-SEARCH-CHAR 
							    #/' STRING (1+ next-ind) (1- TO))
							  (1- TO)))))
				 (#// (if (eq (AREF STRING (1+ next-ind)) #/*)
					  (setq index (+ 2 (or (string-search
								 "*//"
								 STRING
								 (1+ next-ind))
							       (1- TO))))
					  (incf index)))
				 (otherwise (return index)))))
		   
		   (RETURN NIL))
	       
	       (AND (%STRING-EQUAL KEY KEY-FROM STRING FROM KEY-LEN)
		    (RETURN FROM))
	       
	       (SETQ FROM (1+ FROM))		;Avoid infinite loop.
	       (GO LOOP)))))

(DEFUN C-FIND-NEXT-COMMENT-START (LINE &OPTIONAL (INDEX 0))
  "Return a description of where after INDEX on LINE a comment starts, if anywhere.
There are three values: START-START-INDEX, START-END-INDEX, INSIDE-STRING.
The first two are the indices in LINE of the beginning and end
 of the comment starter, or NIL if there is no comment starter.
INSIDE-STRING is non-NIL if this whole line is inside a string.
In that case, you might want to ignore the comment starter even if there is one."
  
  ;;Idea is from FIND-COMMENT-START, but generality has been sacrificed for streamlining.
  ;;Note the absence of such error checks as (and *comment-start* ...)
  ;;??Be sure to implement the inside-string someday
  (DECLARE (RETURN-LIST START-START-INDEX START-END-INDEX INSIDE-STRING))
  
  (let* ((START-START-INDEX (STRING-SEARCH *COMMENT-BEGIN* LINE INDEX))
	 (START-END-INDEX (and START-START-INDEX (+ START-START-INDEX 2)))
	 INSIDE-STRING)				;??Implement this sucker
    
    (values START-START-INDEX START-END-INDEX INSIDE-STRING)))

(DEFUN C-FIND-NEXT-COMMENT-END (LINE &OPTIONAL (INDEX 0))
  "Return a description of where after INDEX on LINE a comment ends, if anywhere.
There are three values: END-START-INDEX, END-END-INDEX, INSIDE-STRING.
The first two are the indices in LINE of the beginning and end
 of the comment ender, or NIL if there is no comment ender.
INSIDE-STRING is non-NIL if this whole line is inside a string.
In that case, you might want to ignore the comment ender even if there is one."
  
  ;;Idea is from FIND-COMMENT-START, but generality has been sacrificed for streamlining.
  ;;Note the absence of such error checks as (and *comment-end* ...)
  (DECLARE (RETURN-LIST END-START-INDEX END-END-INDEX INSIDE-STRING))
  
  (let* ((END-START-INDEX (STRING-SEARCH *COMMENT-END* LINE INDEX))
	 (END-END-INDEX (and END-START-INDEX (+ END-START-INDEX 2)))
	 INSIDE-STRING)				;??Implement this sucker
    
    (values  END-START-INDEX END-END-INDEX INSIDE-STRING)))


(DEFUN C-PARSE-LINE (LINE START-IN-STRING 
		     &OPTIONAL (START-INDEX 0) (END-INDEX (LINE-LENGTH LINE)))
  "Parse LINE's C syntax as it pertains to moving over multiple lines.
START-INDEX and END-INDEX specify a part of LINE to parse.
START-IN-STRING should be the character which will bring us out of the string,
 meaning assume the line starts within such a grouping,
 or NIL meaning assume it does not begin in a string. 
The value returned is a list: the first element is the change in PARENTHESIS-depth 
/(positive means open-parens), the second is the change in BRACE-depth, the third 
is copied from START-IN-STRING, and the fourth is like START-IN-STRING but applies 
to the end of the line instead of the start."
;;Oooh,  it really looks like you've got the tail by the throat!
;;Find out what the hell mindepth was used for??
  (DO (ch
       (paren-depth 0)
       (brace-depth 0)
       (comment-depth 0)
       (IN-STRING START-IN-STRING)
       (INDEX START-INDEX (1+ INDEX)))
      
      (( INDEX END-INDEX) (LIST paren-depth brace-depth START-IN-STRING IN-STRING))
    
    (setq CH (LDB %%CH-CHAR (AREF LINE INDEX)))
    
    (COND (IN-STRING				;In a string, the opening char or *comment-end* 
	   (and (= CH IN-STRING)		;can close it. Nothing else matters.
		(or ( CH #/*) (string-equal line "*//" :start1 index :end1 (+ index 2)))
		(SETQ IN-STRING NIL)))
	  
	  ((= LIST-DOUBLE-QUOTE (LIST-SYNTAX-OPEN-CODED CH))	;String-starting char
	   (SETQ IN-STRING CH))
	  
	  ((string-equal line "//*" :start1 index :end1 (+ index 2))
	   (SETQ IN-STRING #/*)			;The 1st char of *comment-end*
	   (incf comment-depth)
	   (incf index))
	  
	  ((= CH #/() (incf paren-depth))	;Opens
	  ((= CH #/{) (incf brace-depth))
	  ((= CH #/)) (incf paren-depth -1))	;Closes
	  ((= CH #/}) (incf brace-depth -1))
	  ((= CH #/\) (incf INDEX))		;Skip an escape char
	  )))

(defun C-parse-line-memoized (LINE START-IN-STRING
			      &OPTIONAL (START-INDEX 0) (END-INDEX (LINE-LENGTH LINE))
			      &AUX TEM)
  "Like C-PARSE-LINE but remember the result and reuse it if called again.
  It remembers its parsing of a line as the C-PARSE-LINE property
in the LINE-CONTENTS-PLIST of the line."
   ;; Memoizing is only done when the entire line is being parsed.
   ;; Because START-IN-STRING is remembered as part of the value,
   ;; we can tell whether the remembered value is for the same
   ;; setting of START-IN-STRING as we are now using.
   ;; The LINE-CONTENTS-PLIST of a line is cleared when the line is munged.
   ;; Thus, if we have a remembered value, we know it matches the current contents.
  (COND ((AND (ZEROP START-INDEX)
	      (= END-INDEX (LINE-LENGTH LINE))
	      ;; If we are parsing the whole string, and there is a remembered value for this string,
	      (SETQ TEM (GET (LOCF (LINE-CONTENTS-PLIST LINE)) 'C-PARSE-LINE))
	      ;; and it used the same START-IN-STRING as we are using now, then just return it.
	      (EQUAL START-IN-STRING (third TEM))))
	;; Otherwise, reparse the line
	(T (SETQ TEM (C-PARSE-LINE LINE START-IN-STRING START-INDEX END-INDEX))
	   ;; and, if we are parsing the entire line, remember the result.
	   (AND (ZEROP START-INDEX)
		(= END-INDEX (LINE-LENGTH LINE))
		(PUTPROP (LOCF (LINE-CONTENTS-PLIST LINE)) TEM 'C-PARSE-LINE))))
  TEM)

(DEFUN C-PARSE-FROM-DEFUN (LINE &OPTIONAL DEFUN-BEG &AUX TEM)
  "Return an indication of whether LINE begins inside a string.
This is done by parsing down from the beginning of the containing defun.
If DEFUN-BEG is non-NIL, it is used as the parse starting point.
The value can be passed to LISP-PARSE-LINE as the START-IN-STRING arg."
  (COND (*C-PARSE-PREPARSED-FLAG*
	  (AND (SETQ LINE (LINE-PREVIOUS LINE))
	       (CONSP (SETQ TEM (GET (LOCF (LINE-CONTENTS-PLIST LINE)) 'C-PARSE-LINE)))
	       (fourth TEM)))
	(T 
	  (OR DEFUN-BEG (SETQ DEFUN-BEG (C-FORWARD-DEFUN (BEG-OF-LINE LINE) -1 T)))
	  (DO ((LINE1 (BP-LINE DEFUN-BEG) (LINE-NEXT LINE1))
	       (START-INDEX (BP-INDEX DEFUN-BEG) 0)
	       (IN-STRING))
	      ((EQ LINE LINE1) IN-STRING)
	    (SETQ TEM (C-PARSE-LINE-MEMOIZED LINE1 IN-STRING START-INDEX))
	    (SETQ IN-STRING
		  (AND (CONSP TEM) (fourth TEM)))))))


(DEFUN C-FORWARD-DEFUN (BP &OPTIONAL (TIMES 1) FIXUP-P)
  "Return a bp which is forward across TIMES defuns from BP.
If BP is within a defun, that is included in the count.
TIMES negative means move backwards.
FIXUP-P non-NIL means if go past beginning or end return a bp
to there					; otherwise return NIL in that case."
 (COND ((ZEROP TIMES) (COPY-BP BP))
      ((PLUSP TIMES)
       (DO-NAMED LUPO
		 ((I 0 (1+ I)))
		 (( I TIMES)
		  BP)
	 (DO () (NIL)
	   (SETQ BP (BEG-LINE BP 1))
	   (COND ((NULL BP)
		  (RETURN-FROM LUPO (IF FIXUP-P
					(COPY-BP (INTERVAL-LAST-BP *INTERVAL*))
					NIL)))
		 ((c-section-p (bp-line BP) nil)
		  (RETURN NIL))))))		;This is not a failure, but a successful return.
      (T
       (DO-NAMED LUPO
		 ((I 0 (1- I)))
		 (( I TIMES)
		  BP)
	 (DO ((FIRSTP T NIL)) (NIL)
	   (SETQ BP (BEG-LINE BP (IF (AND FIRSTP (NOT (BEG-LINE-P BP)))
				     0
				     -1)))
	   (COND ((NULL BP)
		  (RETURN-FROM LUPO (IF FIXUP-P
					(COPY-BP (INTERVAL-FIRST-BP *INTERVAL*))
					NIL)))
		 ((c-section-p (bp-line BP) nil)
		  (RETURN NIL))))))))		;As above.


;;@@@Serious code above, junk below.
#||(After checking for an unmatched open-paren,
	  search backward for a keyword (IF WHILE FOR #/} #/;)
	  if we find a keyword, make its line the model-line,
	  otherwise use (line-previous-non-blank-or-comment ...)
	  )||#

(defun C-FWI-indent (line)
  "If the current line should take its indentation cue from a 
FOR, WHILE, or IF /(hence /"FWI/"/), then a BP to the keyword?? 
is returned, otherwise NIL is returned."
  
  (do* ((BP (end-of-line (line-previous line)) (dbp BP))	;Start at end of prev-line.
	(char (and BP (ldb %%ch-char (bp-char BP)))	;Strip font, checking for valid BP first.
	      (and BP (ldb %%ch-char (bp-char BP))))
	
	(found-keyword (and (memq char '(#/f #/w #/i #/F #/W #/I)) ;;Efficiency- usually we 
			    (or (looking-at BP "FOR")
				(looking-at BP "WHILE")
				(looking-at BP "IF")))
		       (and (memq char '(#/f #/w #/i #/F #/W #/I)) ;;report failure right here.
			    (or (looking-at BP "FOR")
				(looking-at BP "WHILE")
				(looking-at BP "IF")))))
       
       ((or (eq char #/;)			;Quit upon reaching a semi-colon,
	    (eq char #/()			; open-paren,
	    (not BP)				; the beg of buffer,
	    found-keyword)			; or a keyword.
	(when found-keyword  BP))
    
    (when (= char #/))				;Skip over any complete parenthetical expression.
      (ibp BP)					;This puts us in position for (forwrd-sexp ...)
      (move-bp BP (forward-sexp BP -1)))
    (c-skip-string BP nil char)			;Likewise any string or comment.
;    (princ (ascii char))					   ;diagnostic-- kill it when no longer needed.
    ))



(defun c-skip-string (BP forward-p &optional (char (ldb %%ch-char (bp-char BP)))
		      &aux (comments-nest
			     (GET (LOCF (NODE-PROPERTY-LIST *interval*)) :comments-nest)))
  "If BP is on a string-delimiter /(C comments are strings/),
move BP across the string /(the first arg is altered, NOT just copied/). 
Otherwise the BP is returned unchanged.
N.B. If you supply CHAR, it had better be correct, i.e. /(ldb %%ch-char /(bp-char BP/)/).
Also, this is NOT like M-C-B, where the BP initially points after a delimiter."
  (when (memq char '(#/" #// #/*))		;Cut out quickly most of the time.
    ;;Kludge needed because NIL in a file's plist comes out :NIL, which is non-NIL and blows conds!
    (when comments-nest (setq comments-nest (neq comments-nest :nil)))
    (cond ((= char #/") (move-bp BP (forward-sexp BP (if forward-p 1 -1))))
	  ((or (looking-at BP "//*")		;Comment open.
	       (and (= (ldb %%ch-char (bp-char BP)) #/*)
		    (= (ldb %%ch-char (bp-char-before BP)) #//)	;= #/cr at beg of buffer.
		    (dbp BP)))			;Get us aimed right before the comment-open.
	   (when forward-p			;Otherwise, we are where we want to be.
	     (if comments-nest
		 (do ((EOB (interval-last-bp *interval*))
		      (depth 0)
		      (at-open (looking-at BP "//*") (looking-at BP "//*"))
		      (at-close (looking-at BP "*//") (looking-at BP "*//")))
		     ((or (and at-close (= depth 1))	;Coming out of the last comment-close.
			  (BP-= BP EOB))	;Hit the end of the buffer-- give up.
		      (ibp BP) (ibp BP))	;Move beyond the comment-close and return that BP.
		   (when at-open  (incf depth) (ibp BP))
		   (when at-close (incf depth -1) (ibp BP))
		   (ibp BP))
		 ;;Else...
		 (ibp BP) (ibp BP) (ibp BP) (ibp BP)	;Kludge, to prevent seeing /*/ as /**/.
		 (do ((EOB (interval-last-bp *interval*)))
		     ((or (looking-at-backward BP "*//") (BP-= BP EOB))
		      BP)
		   (ibp BP)))))
	  
	  ((or (looking-at BP "*//")		;Comment close.
	       (and (= (ldb %%ch-char (bp-char BP)) #//)
		    (= (ldb %%ch-char (bp-char-before BP)) #/*)
		    (dbp BP)))			;Get us aimed right before the comment-close.
	   (ibp BP) (ibp BP)			;Get us aimed right after the comment-close.
	   (unless forward-p			;Otherwise, we are where we want to be.
	     (if comments-nest
		 (do ((beg-OB (interval-first-bp *interval*))
		      (depth 0)
		      (at-open (looking-at-backward BP "//*") (looking-at-backward BP "//*"))
		      (at-close (looking-at-backward BP "*//")(looking-at-backward BP "*//")))
		     ((or (and at-open (= depth 1))	;Coming out of the last comment-open.
			  (BP-= BP beg-OB))	;Hit the beg of the buffer-- give up.
		      (dbp BP) (dbp BP))	;Move beyond the comment-open and return that BP.
		   (when at-open  (incf depth -1) (dbp BP))
		   (when at-close (incf depth) (dbp BP))
		   (dbp BP))
		 ;;Else...
		 (dbp BP) (dbp BP) (dbp BP) (dbp BP)	;Kludge, to prevent seeing /*/ as /**/.
		 (do ((beg-OB (interval-last-bp *interval*)))
		     ((or (looking-at BP "//*") (BP-= BP beg-OB))
		      BP)
		   (dbp BP))))))
    ))

(defun C-LINE-OPENS-PARENS (line)		;probably will never need this- maybe kill it.
  "T if LINE contains open parens that are not matched within LINE."
  ;;Not finished.
  (do ((i (if (eq line (bp-line (interval-first-bp *interval*)))	;What is this??
	      (bp-index (interval-first-bp *interval*))
	      0)
	  (1+ i))
       (lim (if (eq line (bp-line (interval-last-bp *interval*)))	;ditto??
		(bp-index (interval-last-bp *interval*))
		(line-length line)))
       (state 'normal)
       (level 0))
      (( i lim) (> level 0))
    (let* ((ch (ldb %%ch-char (aref line i)))
	   (syntax (list-syntax ch)))
      (selectq state
	(string (select syntax
		  (list-double-quote (setq state 'normal))
		  (list-slash (setq i (1+ i)))))
	(normal (select syntax
		  (list-slash (setq i (1+ i)))
		  (list-double-quote (setq state 'string))
		  (list-close (setq level (max (1- level) 0)))
		  (list-open (setq level (1+ level)))))))))

(defun jhb-relevant-func-name ()
  "Preliminary testing func (and fury) for c-mode m-.
Should move back to the most recent function call in C-mode."
  ;;This can be much simpler than the LISP version because C has only one style
  ;;of function call we worry about. Operators are function calls, but we will never
  ;;want to see the code for them.
  ;;Okay. Here's the plan: look right around the point to get the nearest symbol.
  ;;If it is not fbound, reach back to the nearest #\( and start again?? No we want to move over 
  ;;C-expressions, not into them, as we move backwards. No sweat. If we cannot get one at the 
  ;;current level, go up a level. But when we reach the top level, punt.
  ;;
  (do* ((point (copy-bp (point)) (FORWARD-CHAR (search point #/( t) 1 T))
	(line (bp-line point) (bp-line point))
	(index (bp-index point) (bp-index point))	;Don't point at the paren-- rather the func.
	;;eventually do this from anywhere in the word.
	(symbol (bp-read-object point) (bp-read-object point)))
						;(string-search #\space line index)
       ((fboundp symbol) symbol)
    
    (format t "~%~3A  ~A" (fboundp symbol) symbol)))


(defun C-NET-PAREN-DEPTH (line)
  ;;Teach it to ignore slashified chars and quotes.
  ;;
  ;;We need to consider whether we are in a quote or not.
  ;;For now we will do without info from the previous line.
  ;;Look for BACKslash?? Ubique.
  
  (do (tbp
       (index 0)				;Can this stay ||?
       (delta-depth 0)
       (next-char-ind (string-search-set '(#/( #/) #/" #//) line)
		      (string-search-set '(#/( #/) #/" #//) line index)))
      
      
      ((not next-char-ind) delta-depth)		;When ~ any more interesting chars on this line...
    (setq tbp (create-bp line next-char-ind))	;Is this the place to do this? or in a (do*...)?
    (print tbp)
    (case (bp-char tbp)
	  (#/" (setq index (1+ (or (string-search #/" line (1+ next-char-ind))
				   (1- (string-length line))))))
	  (#// (ibp tbp t)
	       (setq index (1+ (if (eq (bp-char tbp) #/*)
				   (or (string-search *comment-end* line (1+ next-char-ind))
				       (1- (string-length line)))
				   index))))
	  (#/( (incf delta-depth)
	       (setq index (1+ next-char-ind)))
	  (#/) (incf delta-depth -1)
	       (setq index (1+ next-char-ind))))))

(defun c-net-comment-depth (line)		;LOW priority
  "Return a fixnum showing net change in comment depth."
  ;;Questions  of my childhood:
  ;;   A- Does C allow comments to be nested? or does it really ignore EVERYthing up until
  ;;          the next "*/"  ans: Got to do it both ways. Find the switch on the plist of the file.
  ;;   B- Should we go for efficiency and assume that only one comment is on a given line? ans: NO.
  ;;      if we go for efficiency and someone puts multiple /**/s on one line, (s)he will hate us forever. 
  ;;
  ;;If comments are allowed to be nested,
  ;;    Return a number (n or 0) showing net change in comment depth.
  ;;Otherwise, just tell us if the current line ends in mid-comment or not.
  ;;We should be able to use this same function for string as well as comments. In fact, it is a
  ;;great idea for the editor to treat a comment as a string.
  ;;
  ;;This code has to know about the previous line, which has to know.... we have to parse!!
  ;;Must we crawl through all the comments??
  
;  (if ??theswitch				;Find this on the plist.
  (do* ((index 0 (+ end-ind 2))			;We know the comment delimiter is 2 chars.
	(start-ind (c-find-next-comment-start line)
		   (c-find-next-comment-start line index))
	(end-ind (c-find-next-comment-end line (or start-ind index))
		 (c-find-next-comment-end line (or start-ind index))))
       ((not (and start-ind end-ind))
	(cond ((and start-ind (not end-ind))
	       :IN)
	      ((and  end-ind (not start-ind))
	       :OUT)))))

#||(let
     ((opens
	(do ((sub-line (bp-line (point)))
	     (closes-so-far   0 (1+ closes-so-far  ))
	     (pos-of-this-close 0 (string-search *comment-end* sub-line (1+ pos-of-this-close))))
	    ((not pos-of-this-close) (1- closes-so-far))))
      (closes (do ((sub-line (bp-line (point)))
		   (opens-so-far 0 (1+ opens-so-far))
		   (pos-of-this-open 0 (string-search *comment-begin* sub-line (1+ pos-of-this-open))))
		  ((not pos-of-this-open) (1- opens-so-far))))))||#


#||(
;;just the ones which deal with strings in source code, not buffer-lines as strings.
    '(FORWARD-UP-LIST-OR-STRING
       LISP-STRING-BUFFER-MATCH
       PL1-GET-STRING-BACKWARD
       PL1-GET-STRING-FORWARD
       TYPEIN-LINE-CHAR-OR-STRING
       STRING-FROM-OPTION
       HISTORY-ELEMENT-STRING-FUNCTION
       KILL-STRING
       SEARCH-STRING-SET-KLUDGE
       C-SENSITIVE-STRING-SEARCH
       C-SYMBOL-FROM-STRING
       FORWARD-UP-STRING
       PL1-STRING-FIXNUM-P)
    
    
;;Interesting...
    '(EDSTRING
       STRING-INTERVAL
       TAGS-SEARCH-ALTERNATIVE-STRINGS
       POP-UP-EDSTRING)
    )||#
; End of ZCZWEI.LISP