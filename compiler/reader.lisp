(in-package #:vacietis)
(in-readtable vacietis)

(declaim (optimize (debug 3)))

(in-package #:vacietis.c)

(cl:defparameter vacietis::*type-qualifiers*
  #(static const signed unsigned extern auto register volatile))

(cl:defparameter vacietis::*ops*
  #(= += -= *= /= %= <<= >>= &= ^= |\|=| ? |:| |\|\|| && |\|| ^ & == != < > <= >= << >> ++ -- + - * / % ! ~ -> |.| |,|))

(cl:defparameter vacietis::*possible-prefix-ops*
  #(! ~ sizeof - + & * ++ --))

(cl:defparameter vacietis::*ambiguous-ops*
  #(- + & *))

(cl:defparameter vacietis::*assignment-ops*
  #(= += -= *= /= %= <<= >>= &= ^= |\|=|))

(cl:defparameter vacietis::*binary-ops-table*
  #((|\|\||)                            ; or
    (&&)                                ; and
    (|\||)                              ; logior
    (^)                                 ; logxor
    (&)                                 ; logand
    (== !=)
    (< > <= >=)
    (<< >>)                             ; ash
    (+ -)
    (* / %)))

(cl:in-package #:vacietis)

(defvar %in)
(defvar *c-file* nil)
(defvar *line-number*  nil)

;;; a C macro can expand to several statements; READ should return all of them

(defvar *macro-stream*)

;;; error reporting

(define-condition c-reader-error (reader-error) ;; SBCL hates simple-conditions?
  ((c-file      :reader c-file      :initform *c-file*)
   (line-number :reader line-number :initform *line-number*)
   (msg         :reader msg         :initarg  :msg))
  (:report (lambda (condition stream)
             (write-string (msg condition) stream))))

(defun read-error (msg &rest args)
  (error
   (make-condition
    'c-reader-error
    :stream %in
    :msg (format nil
                 "Error reading C stream~@[ from file ~A~]~@[ at line ~A~]: ~?"
                 *c-file* *line-number* msg args))))

;;; basic stream stuff

(defun c-read-char ()
  (let ((c (read-char %in nil)))
    (when (and (eql c #\Newline) *line-number*)
      (incf *line-number*))
    c))

(defun c-unread-char (c)
  (when (and (eql c #\Newline) *line-number*)
    (decf *line-number*))
  (unread-char c %in))

(defmacro loop-reading (&body body)
  `(loop with c do (setf c (c-read-char))
        ,@body))

(defun next-char (&optional (eof-error? t))
  "Returns the next character, skipping over whitespace and comments"
  (loop-reading
     while (case c
             ((nil)                     (when eof-error?
                                          (read-error "Unexpected end of file")))
             (#\/                       (%maybe-read-comment))
             ((#\Space #\Newline #\Tab #\\) t))
     finally (return c)))

(defun make-buffer (&optional (element-type t))
  (make-array 10 :adjustable t :fill-pointer 0 :element-type element-type))

(defun slurp-while (predicate)
  (let ((string-buffer (make-buffer 'character)))
    (loop-reading
       while (and c (funcall predicate c))
       do (unless (or (char= c #\\) (char= c #\Newline))
            (vector-push-extend c string-buffer))
       finally (when c (c-unread-char c)))
    string-buffer))

(defun %maybe-read-comment ()
  (case (peek-char nil %in)
    (#\/ (when *line-number* (incf *line-number*))
         (read-line %in))
    (#\* (slurp-while (let ((previous-char (code-char 0)))
                        (lambda (c)
                          (prog1 (not (and (char= previous-char #\*)
                                           (char= c #\/)))
                            (setf previous-char c)))))
         (c-read-char))))

(defun read-c-comment (%in slash)
  (declare (ignore slash))
  (%maybe-read-comment)
  (values))

;;; numbers

(defun read-octal ()
  (parse-integer (slurp-while (lambda (c) (char<= #\0 c #\7)))
                 :radix 8))

(defun read-hex ()
  (parse-integer
   (slurp-while (lambda (c)
                  (or (char<= #\0 c #\9) (char-not-greaterp #\A c #\F))))
   :radix 16))

(defun read-float (prefix separator)
  (let ((*readtable* (find-readtable :common-lisp)))
    (read-from-string
     (format nil "~d~a~a" prefix separator
             (slurp-while (lambda (c) (find c "0123456789+-eE" :test #'char=)))))))

(defun read-decimal (c0) ;; c0 must be #\1 to #\9
  (labels ((digit-value (c) (- (char-code c) 48)))
    (let ((value (digit-value c0)))
      (loop-reading
           (cond ((null c)
                  (return value))
                 ((char<= #\0 c #\9)
                  (setf value (+ (* 10 value) (digit-value c))))
                 ((or (char-equal c #\E) (char= c #\.))
                  (return (read-float value c)))
                 ((char-equal c #\L)
                  (return value))
                 (t
                  (c-unread-char c)
                  (return value)))))))

(defun read-c-number (c)
  (prog1 (if (char= c #\0)
             (let ((next (peek-char nil %in nil)))
               (if next
                 (if (digit-char-p next 8)
                     (read-octal)
                     (case next
                       ((#\X #\x) (c-read-char) (read-hex))
                       (#\.       (c-read-char) (read-float 0 #\.))
                       (otherwise 0)))
                 0))
             (read-decimal c))
    (loop repeat 2 do (when (find (peek-char nil %in nil nil) "ulf" :test #'eql)
                        (c-read-char)))))

;;; string and chars (caller has to remember to discard leading #\L!!!)

(defun read-char-literal (c)
  (if (char= c #\\)
      (let ((c (c-read-char)))
        (code-char (case c
                     (#\a 7)
                     (#\f 12)
                     (#\n 10)
                     (#\r 13)
                     (#\t 9)
                     (#\v 11)
                     (#\x (read-hex))
                     (otherwise (if (char<= #\0 c #\7)
                                    (progn (c-unread-char c) (read-octal))
                                    (char-code c))))))
      c))

(defun read-character-constant (%in single-quote)
  (declare (ignore single-quote))
  (prog1 (char-code (read-char-literal (c-read-char)))
    (unless (char= (c-read-char) #\')
      (read-error "Junk in character constant"))))

(defun read-c-string (%in double-quotes)
  (declare (ignore double-quotes))
  (let ((string (make-buffer 'character)))
    (loop-reading
       (if (char= c #\") ;; c requires concatenation of adjacent string literals
           (progn (setf c (next-char nil))
                  (unless (eql c #\")
                    (when c (c-unread-char c))
                    (return `(string-to-char* ,string))))
           (vector-push-extend (read-char-literal c) string)))))

;;; preprocessor

(defvar preprocessor-if-stack ())
(defvar *preprocessing* nil)

(defun pp-read-line ()
  (let (comment-follows?)
   (prog1
       (slurp-while (let (backslash-seen)
                      (lambda (c)
                        (case c
                          (#\Newline
                           (when backslash-seen
                             (setf backslash-seen nil)
                             t))
                          (#\\ (setf backslash-seen t))
                          (#\/ (if (find (peek-char nil %in nil nil) "/*")
                                   (progn (setf comment-follows? t) nil)
                                   t))
                          (t t)))))
     (c-read-char)
     (when comment-follows?
       (%maybe-read-comment)))))

(defmacro lookup-define ()
  `(gethash (read-c-identifier (next-char))
            (compiler-state-pp *compiler-state*)))

(defun preprocessor-skip-branch ()
  (let ((if-nest-depth 1))
    (loop for line = (pp-read-line) do
         (cond ((ppcre:scan "# *if" line)
                (incf if-nest-depth))
               ((ppcre:scan "# *ifdef" line)
                (incf if-nest-depth))
               ((ppcre:scan "# *ifndef" line)
                (incf if-nest-depth))
               ((and (ppcre:scan "# *else" line)
                     (= 1 if-nest-depth))
                (return))
               ((and (ppcre:scan "# *endif" line)
                     (= 0 (decf if-nest-depth)))
                (pop preprocessor-if-stack)
                (return))
               ((and (ppcre:scan "# *elif" line)
                     (= 1 if-nest-depth))
                (case (car preprocessor-if-stack)
                  (  if (when (preprocessor-test
                               (subseq line (multiple-value-bind (_ end)
                                                (ppcre:scan "# *elif" line)
                                              (declare (ignore _)) end)))
                        (setf (car preprocessor-if-stack) 'elif)
                        (return)))
                  (elif nil)
                  (else (read-error "Misplaced #elif"))))))))

(defun preprocessor-test (line)
  (let ((*preprocessing* t))
    (let ((exp (with-input-from-string (%in line)
                 (read-infix-exp (read-c-exp (next-char))))))
      (not (eql 0 (eval `(symbol-macrolet
                             ,(let ((x))
                                (maphash (lambda (k v)
                                           (push (list k v) x))
                                         (compiler-state-pp *compiler-state*))
                                x)
                           ,exp)))))))

(defun fill-in-template (args template subs)
  (ppcre:regex-replace-all
   (format nil "([^a-zA-Z])?(~{~a~^|~})([^a-zA-Z0-9])?" args)
   ;; This works, but is more permissive than GCC.  See reader-test
   ;; preprocessor-no-concatenation.
   (ppcre:regex-replace-all "##" template "")
   (lambda (match r1 arg r2)
     (declare (ignore match))
     (format nil "~A~A~A"
             (or r1 "")
             (elt subs (position arg args :test #'string=))
             (or r2 "")))
   :simple-calls t))

(defun c-read-delimited-strings (&optional skip-spaces?)
  (next-char) ;; skip opening paren
  (let ((paren-depth 0)
        (acc (make-buffer)))
    (with-output-to-string (sink)
      (loop for c = (c-read-char)
            until (and (= paren-depth 0) (eql #\) c)) do
            (case c
              (#\Space (unless skip-spaces? (princ c sink)))
              (#\( (incf paren-depth) (princ c sink))
              (#\) (decf paren-depth) (princ c sink))
              (#\, (vector-push-extend (get-output-stream-string sink) acc))
              (otherwise (princ c sink)))
            finally (let ((last (get-output-stream-string sink)))
                      (unless (string= last "")
                        (vector-push-extend last acc)))))
    (map 'list #'identity acc)))

(defun find-include (include-file)
  (dolist (path (compiler-state-include-paths *compiler-state*))
    (let ((include (merge-pathnames
                    (merge-pathnames include-file path)
                    (or *load-truename* *compile-file-truename*
                        *default-pathname-defaults*))))
      (when (cl-fad:file-exists-p include) (return include)))))

(defun read-c-macro (%in sharp)
  (declare (ignore sharp))
  ;; preprocessor directives need to be read in a separate namespace
  (let ((pp-directive (read-c-identifier (next-char))))
    (case pp-directive
      (vacietis.c:define
       (setf (lookup-define)
             (if (eql #\( (peek-char nil %in))
                 (let ((args     (c-read-delimited-strings t))
                       (template (string-trim '(#\Space #\Tab) (pp-read-line))))
                   (lambda (substitutions)
                     (if args
                         (fill-in-template args template substitutions)
                         template)))
                 (pp-read-line))))
      (vacietis.c:undef
       (remhash (read-c-identifier (next-char))
                (compiler-state-pp *compiler-state*))
       (pp-read-line))
      (vacietis.c:include
       (let* ((delimiter
               (case (next-char)
                 (#\" #\") (#\< #\>)
                 (otherwise (read-error "Error reading include path: ~A"
                                        (pp-read-line)))))
              (include-file
               (slurp-while (lambda (c) (char/= c delimiter)))))
         (next-char)
         (if (char= delimiter #\")
             (%load-c-file (merge-pathnames
                            include-file
                            (directory-namestring
                             (or *load-truename* *compile-file-truename*
                                 *default-pathname-defaults*)))
                           *compiler-state*)
             (let ((non-system-include-file (find-include include-file)))
               (if non-system-include-file
                   (%load-c-file non-system-include-file *compiler-state*)
                   (include-libc-file include-file))))))
      (vacietis.c:if
       (push 'if preprocessor-if-stack)
       (unless (preprocessor-test (pp-read-line))
         (preprocessor-skip-branch)))
      (vacietis.c:ifdef
       (push 'if preprocessor-if-stack)
       (unless (lookup-define)
         (preprocessor-skip-branch)))
      (vacietis.c:ifndef
       (push 'if preprocessor-if-stack)
       (when (lookup-define)
         (preprocessor-skip-branch)))
      (vacietis.c:else ;; skip this branch
       (if preprocessor-if-stack
           (progn (setf (car preprocessor-if-stack) 'else)
                  (preprocessor-skip-branch))
           (read-error "Misplaced #else")))
      (vacietis.c:endif
       (if preprocessor-if-stack
           (pop preprocessor-if-stack)
           (read-error "Misplaced #endif")))
      (vacietis.c:elif
       (if preprocessor-if-stack
           (preprocessor-skip-branch)
           (read-error "Misplaced #elif")))
      (otherwise ;; line, pragma, error ignored for now
       (pp-read-line))))
  (values))

;;; types and size-of

(defun type-qualifier? (x)
  (find x *type-qualifiers*))

(defun basic-type? (x)
  (find x *basic-c-types*))

(defun c-type? (identifier)
  ;; and also do checks for struct, union, enum and typedef types
  (or (type-qualifier? identifier)
      (basic-type?     identifier)
      (find identifier #(vacietis.c:struct vacietis.c:enum))
      (gethash identifier (compiler-state-typedefs *compiler-state*))))

(defvar *local-var-types* nil)

(defun size-of (x)
  (or (type-size x)
      (type-size (gethash x (or *local-var-types*
                                (compiler-state-var-types *compiler-state*))))))

;;; infix

(defun parse-infix (exp &optional (start 0) (end (when (vectorp exp) (length exp))))
  (if (vectorp exp)
      (block nil
        (when (= 0 (length exp))
          (return))
        (when (= 1 (- end start))
          (return (parse-infix (aref exp start))))
        (labels ((cast? (x)
                   (and (vectorp x) (some #'c-type? x)))
                 (match-binary-ops (table &key (lassoc t))
                   (position-if (lambda (x) (find x table))
                                exp :start (1+ start) :end (1- end)
                                :from-end lassoc))
                 (parse-binary (i &optional op)
                   (list (or op (aref exp i))
                         (parse-infix exp start  i)
                         (parse-infix exp (1+ i) end))))
          ;; in order of weakest to strongest precedence
          ;; comma
          (awhen (match-binary-ops '(vacietis.c:|,|))
            (return (parse-binary it 'progn)))
          ;; assignment
          (awhen (match-binary-ops *assignment-ops* :lassoc nil)
            (return (parse-binary it)))
          ;; elvis
          (awhen (position 'vacietis.c:? exp :start start :end end)
            (let ((?pos it))
              (return
                `(if (not (eql 0 ,(parse-infix exp start ?pos)))
                     ,@(aif (position 'vacietis.c:|:| exp :start ?pos :end end)
                            (list (parse-infix exp (1+ ?pos) it)
                                  (parse-infix exp (1+ it)   end))
                            (read-error "Error parsing ?: trinary operator in: ~A"
                                        (subseq exp start end)))))))
          ;; various binary operators
          (loop for table across *binary-ops-table* do
               (awhen (match-binary-ops table)
                 (if (and (find (elt exp it) *ambiguous-ops*)
                          (let ((prev (elt exp (1- it))))
                            (or (find prev *ops*) (cast? prev))))
                     (awhen (position-if (lambda (x)
                                           (not (or (find x *ops*)
                                                    (cast? x))))
                                         exp
                                         :start     start
                                         :end          it
                                         :from-end      t)
                       (return-from parse-infix (parse-binary (1+ it))))
                     (return-from parse-infix (parse-binary it)))))
          ;; unary operators
          (flet ((parse-rest (i)
                   (parse-infix exp (1+ i) end)))
            (loop for i from start below end for x = (aref exp i) do
                 (cond ((cast? x)                               ;; cast
                        (return-from parse-infix (parse-rest i)))
                       ((find x #(vacietis.c:++ vacietis.c:--)) ;; inc/dec
                        (return-from parse-infix
                          (let* ((postfix? (< start i))
                                 (place    (if postfix?
                                               (parse-infix exp start  i)
                                               (parse-infix exp (1+ i) end)))
                                 (set-exp `(vacietis.c:=
                                            ,place
                                            (,(if (eq x 'vacietis.c:++)
                                                  'vacietis.c:+
                                                  'vacietis.c:-)
                                              ,place 1))))
                            (if postfix?
                                `(prog1 ,place ,set-exp)
                                set-exp))))
                       ((find x *possible-prefix-ops*)          ;; prefix op
                        (return-from parse-infix
                          (if (eq x 'vacietis.c:sizeof)
                              (let ((type-exp (aref exp (1+ i))))
                                (when (vectorp type-exp) ;; fixme
                                  (setf type-exp (aref type-exp 0)))
                                (or (size-of type-exp)
                                    (read-error "Don't know sizeof ~A" type-exp)))
                              (list (case x
                                      (vacietis.c:- '-)
                                      (vacietis.c:* 'vacietis.c:deref*)
                                      (vacietis.c:& 'vacietis.c:mkptr&)
                                      (otherwise     x))
                                    (parse-rest i))))))))
          ;; funcall, aref, and struct access
          (loop for i from (1- end) downto (1+ start) for x = (aref exp i) do
               (cond
                 ((find x #(vacietis.c:|.| vacietis.c:->))
                  (let ((exp (parse-binary i)))
                    (return-from parse-infix
                      `(vacietis.c:|.|
                        ,(if (eq x 'vacietis.c:->)
                             `(vacietis.c:deref* ,(elt exp 1))
                             (elt exp 1))
                        ,(gethash (elt exp 2)
                                  (compiler-state-accessors *compiler-state*))))))
                 ((listp x) ;; aref
                  (return-from parse-infix
                    (if (eq (car x) 'vacietis.c:[])
                        `(vacietis.c:[] ,(parse-infix exp start i)
                                        ,(parse-infix (second x)))
                        (read-error "Unexpected list when parsing ~A" exp))))
                 ((vectorp x) ;; funcall
                  (return-from parse-infix
                    (let ((fun-exp (parse-infix exp start i)))
                     (append
                      (if (symbolp fun-exp)
                          (list fun-exp)
                          (list 'funcall fun-exp))
                      (loop with xstart = 0
                            for next = (position 'vacietis.c:|,| x :start xstart)
                            when (< 0 (length x))
                              collect (parse-infix x xstart (or next (length x)))
                            while next do (setf xstart (1+ next)))))))))
          (read-error "Error parsing expression: ~A" (subseq exp start end))))
      exp))

;;; statements

(defun read-c-block (c)
  (if (eql c #\{)
      (loop for c = (next-char)
            until (eql c #\}) append (reverse
                                      (multiple-value-list
                                       (read-c-statement c))))
      (read-error "Expected opening brace '{' but found '~A'" c)))

(defun next-exp ()
  (read-c-exp (next-char)))

(defvar *variable-declarations*)
(defvar *cases*)

(defun read-exps-until (predicate)
  (let ((exps (make-buffer)))
    (loop for c = (next-char)
          until (funcall predicate c)
          do (vector-push-extend (read-c-exp c) exps))
    exps))

(defun c-read-delimited-list (open-delimiter separator)
  (let ((close-delimiter (ecase open-delimiter (#\( #\)) (#\{ #\}) (#\; #\;)))
        (list            (make-buffer))
        done?)
    (loop until done? do
         (vector-push-extend
          (read-exps-until (lambda (c)
                             (cond ((eql c close-delimiter) (setf done? t))
                                   ((eql c separator)       t))))
          list))
    list))

(defun read-control-flow-statement (statement)
  (flet ((read-block-or-statement ()
           (let ((next-char (next-char)))
             (if (eql next-char #\{)
                 (cons 'tagbody (read-c-block next-char))
                 (read-c-statement next-char)))))
    (if (eq statement 'vacietis.c:if)
        (let* ((test       (parse-infix (next-exp)))
               (then       (read-block-or-statement))
               (next-char  (next-char nil))
               (next-token (case next-char
                             (#\e  (read-c-exp #\e))
                             ((nil))
                             (t    (c-unread-char next-char) nil)))
               (if-exp    `(if (eql 0 ,test)
                               ,(when (eq next-token 'vacietis.c:else)
                                      (read-block-or-statement))
                               ,then)))
          (if (or (not next-token) (eq next-token 'vacietis.c:else))
              if-exp
              `(progn ,if-exp ,(%read-c-statement next-token))))
        (case statement
          ((vacietis.c:break vacietis.c:continue)
            `(go ,statement))
          (vacietis.c:goto
            `(go ,(read-c-statement (next-char))))
          (vacietis.c:return
            `(return ,(or (read-c-statement (next-char)) 0)))
          (vacietis.c:case
            (prog1 (car (push
                         (eval (parse-infix (next-exp))) ;; must be constant int
                         *cases*))
              (unless (eql #\: (next-char))
                (read-error "Error parsing case statement"))))
          (vacietis.c:switch
            (let* ((exp     (parse-infix (next-exp)))
                   (*cases* ())
                   (body    (read-c-block (next-char))))
              `(vacietis.c:switch ,exp ,*cases* ,body)))
          (vacietis.c:while
            `(vacietis.c:for (nil nil ,(parse-infix (next-exp)) nil)
               ,(read-block-or-statement)))
          (vacietis.c:do
            (let ((body (read-block-or-statement)))
              (if (eql (next-exp) 'vacietis.c:while)
                  (prog1 `(vacietis.c:do ,body ,(parse-infix (next-exp)))
                    (read-c-statement (next-char))) ;; semicolon
                  (read-error "No 'while' following a 'do'"))))
          (vacietis.c:for
            `(vacietis.c:for
                 ,(let* ((*local-var-types*       (make-hash-table))
                         (*variable-declarations* ()) ;; c99, I think?
                         (initializations         (progn
                                                    (next-char)
                                                    (read-c-statement
                                                     (next-char)))))
                        (list* *variable-declarations*
                               initializations
                               (map 'list
                                    #'parse-infix
                                    (c-read-delimited-list #\( #\;))))
               ,(read-block-or-statement)))))))

(defun read-function (name result-type)
  (declare (ignore result-type))
  (let (arglist)
    (block done-arglist
      (loop for param across (c-read-delimited-list (next-char) #\,) do
           (block done-arg
             (labels ((strip-type (x)
                        (cond ((symbolp x)
                               (push x arglist)
                               (return-from done-arg))
                              ((vectorp x)
                               (loop for x1 across x do
                                    (when (not (or (c-type? x1)
                                                   (eq 'vacietis.c:* x1)))
                                      (strip-type x1))))
                              (t
                               (read-error
                                "Junk in argument list: ~A" x)))))
               (loop for x across param do
                    (cond
                      ((eq x 'vacietis.c:|.|)
                       (progn (push '&rest            arglist)
                              (push 'vacietis.c:|...| arglist)
                              (return-from done-arglist)))
                      ((not (or (c-type? x) (eq 'vacietis.c:* x)))
                       (strip-type x))))))))
    (if (eql (peek-char nil %in) #\;)
        (prog1 t (c-read-char)) ;; forward declaration
        `(defun/1 ,name ,(reverse arglist)
           ,(let* ((*variable-declarations* ())
                   (*local-var-types*       (make-hash-table))
                   (body                    (read-c-block (next-char))))
              `(prog* ,*variable-declarations*
                  ,@body))))))

(defun process-variable-declaration (spec base-type)
  (let (name (type base-type) initial-value init-size)
    (labels ((init-object (value)
               (if (vector-literal-p value)
                   (let ((els (cons 'vector (vector-literal-elements value))))
                     (if (struct-type-p type)
                         els
                         (progn (setf init-size (length els))
                                `(vacietis::make-memptr :mem ,els))))
                   (progn
                     (when (and (listp value) (eq 'string-to-char* (car value)))
                       (setf init-size (length (second value))))
                     value)))
             (parse-declaration (x)
               (if (symbolp x)
                   (setf name x)
                   (destructuring-bind (qualifier name1 &optional val/size)
                       x
                     (setf name name1)
                     (case qualifier
                       (vacietis.c:=
                        (setf initial-value (init-object val/size))
                        (parse-declaration name1))
                       (vacietis.c:[]
                        (setf type
                              (make-array-type
                               :element-type type
                               :dimensions   (awhen (or val/size init-size)
                                               (list it))))
                        (parse-declaration name))
                       (vacietis.c:deref*
                        (setf type (make-pointer-to :type type))
                        (parse-declaration name))
                       (t (read-error "Unknown thing in declaration ~A" x)))))))
      (parse-declaration spec)
      (values name type initial-value))))

(defun read-variable-declarations (spec-so-far base-type)
  (let ((decls      (c-read-delimited-list #\; #\,))
        (decl-code  ()))
    (setf (aref decls 0) (concatenate 'vector spec-so-far (aref decls 0)))
    (loop for x across decls do
         (multiple-value-bind (name type initial-value)
             (process-variable-declaration (parse-infix x) base-type)
           (setf (gethash name (or *local-var-types*
                                   (compiler-state-var-types *compiler-state*)))
                 type)
           (if (boundp '*variable-declarations*)
               (progn (push (list name (preallocated-value-exp-for type))
                            *variable-declarations*)
                      (when initial-value
                        (push `(vacietis.c:= ,name ,initial-value)
                              decl-code)))
               (push `(defparameter ,name
                        ,(or initial-value
                             (preallocated-value-exp-for type)))
                     decl-code))))
    (if decl-code
        (cons 'progn (reverse decl-code))
        t)))

(defun read-var-or-function-declaration (base-type)
  "Reads a variable(s) or function declaration"
  (let ((type base-type)
        name
        (spec-so-far (make-buffer)))
    (loop for c = (next-char) do
         (cond ((eql c #\*)
                (setf type        (make-pointer-to :type type))
                (vector-push-extend 'vacietis.c:* spec-so-far))
               ((or (eql c #\_) (alpha-char-p c))
                (setf name (read-c-identifier c))
                (vector-push-extend name spec-so-far)
                (return))
               (t
                (c-unread-char c)
                (return))))
    (let ((next (next-char)))
      (c-unread-char next)
      (if (and name (eql #\( next))
          (read-function name type)
          (read-variable-declarations spec-so-far base-type)))))

(defun read-typedef (base-type)
  (multiple-value-bind (name type)
      (process-variable-declaration (read-infix-exp (next-exp)) base-type)
    (setf (gethash name (compiler-state-typedefs *compiler-state*)) type)
    t))

(defun read-enum-decl ()
  (when (eql #\{ (peek-char t %in))
    (next-char)
    (let ((enums (c-read-delimited-list #\{ #\,)))
      ;; fixme: assigned values to enum names
      (loop for name across enums for i from 0 do
           (setf (gethash (elt name 0) (compiler-state-enums *compiler-state*))
                 i))))
  (if (eql #\; (peek-char t %in))
      (progn (next-char) t)
      (read-variable-declarations #() 'vacietis.c:int)))

(defun read-base-type (token)
  (loop while (type-qualifier? token)
        do    (setf token (next-exp)))
  (awhen (gethash token (compiler-state-typedefs *compiler-state*))
    (setf token it))
  (cond ((eq token 'vacietis.c:enum)
         (make-enum-type :name (next-exp)))
        ((eq token 'vacietis.c:struct)
         (if (eql #\{ (peek-char t %in))
             (progn
               (c-read-char)
               (read-struct-decl-body (make-struct-type)))
             (let ((name (next-exp)))
               (or (gethash name (compiler-state-structs *compiler-state*))
                   (make-struct-type :name name)))))
        ((or (basic-type? token) (c-type-p token))
         token)
        (t
         (read-error "Unexpected parser error: unknown type ~A" token))))

(defun read-struct-decl-body (struct-type)
  (let ((i 0))
    (loop for c = (next-char) until (eql #\} c) do
         (multiple-value-bind (slot-name slot-type)
             (let ((base-type (read-base-type (read-c-exp c))))
               (process-variable-declaration (read-infix-exp (next-exp))
                                             base-type))
           (setf (gethash slot-name
                          (compiler-state-accessors *compiler-state*))
                 i
                 (struct-type-slots struct-type)
                 (append (struct-type-slots struct-type) (list slot-type)))
           (incf i (size-of slot-type))))))

(defun read-struct (struct-type)
  (acase (next-char)
    (#\{ (read-struct-decl-body struct-type)
         (awhen (struct-type-name struct-type)
           (setf (gethash it (compiler-state-structs *compiler-state*))
                 struct-type))
         (let ((c (next-char)))
           (if (eql #\; c)
               t
               (progn (c-unread-char c)
                      (read-variable-declarations #() struct-type)))))
    (#\; t) ;; forward declaration
    (t   (read-variable-declarations (vector (read-c-exp it))
                                     struct-type))))

(defun read-declaration (token)
  (cond ((eq 'vacietis.c:typedef token)
         (read-typedef (read-base-type (next-exp))))
        ((c-type? token)
         (let ((base-type (read-base-type token)))
           (cond ((struct-type-p base-type)
                  (read-struct base-type))
                 ((enum-type-p base-type)
                  (read-enum-decl))
                 (t
                  (read-var-or-function-declaration base-type)))))))

(defun read-labeled-statement (token)
  (when (eql #\: (peek-char t %in))
    (next-char)
    (values (read-c-statement (next-char)) token)))

(defun read-infix-exp (next-token)
  (let ((exp (make-buffer)))
    (vector-push-extend next-token exp)
    (loop for c = (next-char nil)
          until (or (eql c #\;) (null c))
          do (vector-push-extend (read-c-exp c) exp))
    (parse-infix exp)))

(defun %read-c-statement (token)
  (multiple-value-bind (statement label) (read-labeled-statement token)
    (acond (label                    (values statement label))
           ((read-declaration token) (if (eq t it) (values) it))
           (t                        (or (read-control-flow-statement token)
                                         (read-infix-exp token))))))

(defun read-c-statement (c)
  (case c
    (#\# (read-c-macro %in c))
    (#\; (values))
    (t   (%read-c-statement (read-c-exp c)))))

(defun read-c-identifier (c)
  ;; assume inverted readtable (need to fix for case-preserving lisps)
  (let* ((raw-name (concatenate
                    'string (string c)
                    (slurp-while (lambda (c)
                                   (or (eql c #\_) (alphanumericp c))))))
         (raw-name-alphas (remove-if-not #'alpha-char-p raw-name))
         (identifier-name
          (format nil
                  (cond ((every #'upper-case-p raw-name-alphas) "~(~A~)")
                        ((every #'lower-case-p raw-name-alphas) "~:@(~A~)")
                        (t "~A"))
                  raw-name)))
    (or (find-symbol identifier-name '#:vacietis.c) (intern identifier-name))))

(defun match-longest-op (one)
  (flet ((seq-match (&rest chars)
           (find (make-array (length chars)
                             :element-type 'character
                             :initial-contents chars)
                 *ops* :test #'string= :key #'symbol-name)))
    (let ((one-match (seq-match one))
          (two (c-read-char)))
      (acond ((null two)
              one-match)
             ((seq-match one two)
              (let ((three-match (seq-match one two (peek-char nil %in))))
                (if three-match
                    (progn (c-read-char) three-match)
                    it)))
             (t (c-unread-char two) one-match)))))

(defstruct vector-literal
  elements)

(defun read-vector-literal ()
  (make-vector-literal
   :elements (map 'list #'parse-infix (c-read-delimited-list #\{ #\,))))

(defun read-c-exp (c)
  (or (match-longest-op c)
      (cond ((digit-char-p c) (read-c-number c))
            ((or (eql c #\_) (alpha-char-p c))
             (let ((symbol (read-c-identifier c)))
               (acond
                 ((gethash symbol (compiler-state-pp *compiler-state*))
                  (setf *macro-stream*
                        (make-string-input-stream
                         (etypecase it
                           (string
                            it)
                           (function
                            (funcall it (c-read-delimited-strings)))))
                        %in
                        (make-concatenated-stream *macro-stream* %in))
                  (read-c-exp (next-char)))
                 ((gethash symbol (compiler-state-enums *compiler-state*))
                  it)
                 (t
                  (if *preprocessing*
                      (if (string-equal symbol "defined")
                          (if (lookup-define) 1 0)
                          (if (gethash symbol
                                       (compiler-state-pp *compiler-state*))
                              symbol
                              0))
                      symbol)))))
            (t
             (case c
               (#\" (read-c-string %in c))
               (#\' (read-character-constant %in c))
               (#\( (read-exps-until (lambda (c) (eql #\) c))))
               (#\{ (read-vector-literal)) ;; decl only
               (#\[ (list 'vacietis.c:[]
                          (read-exps-until (lambda (c) (eql #\] c))))))))))

;;; readtable

(defun read-c-toplevel (%in c)
  (let* ((*macro-stream* nil)
         (exp1           (read-c-statement c)))
    (if (and *macro-stream* (peek-char t *macro-stream* nil))
        (list* 'progn
               exp1
               (loop while (peek-char t *macro-stream* nil)
                     collect (read-c-statement (next-char))))
        (or exp1 (values)))))

(defun read-c-newline (%in c)
  (when *line-number* (incf *line-number*))
  (values))

(macrolet
    ((def-c-readtable ()
       `(defreadtable c-readtable
         (:case :invert)

         ;; unary and prefix operators
         ,@(loop for i in '(#\+ #\- #\~ #\! #\( #\& #\*)
              collect `(:macro-char ,i 'read-c-toplevel nil))

         (:macro-char #\Newline 'read-c-newline nil)

         (:macro-char #\# 'read-c-macro nil)

         (:macro-char #\/ 'read-c-comment nil)

         (:macro-char #\" 'read-c-string nil)
         (:macro-char #\' 'read-character-constant nil)

         ;; numbers (should this be here?)
         ,@(loop for i from 0 upto 9
              collect `(:macro-char ,(digit-char i) 'read-c-toplevel nil))

         ;; identifiers
         (:macro-char #\_ 'read-c-toplevel nil)
         ,@(loop for i from (char-code #\a) upto (char-code #\z)
              collect `(:macro-char ,(code-char i) 'read-c-toplevel nil))
         ,@(loop for i from (char-code #\A) upto (char-code #\Z)
              collect `(:macro-char ,(code-char i) 'read-c-toplevel nil))
         )))
  (def-c-readtable))

(defvar c-readtable (find-readtable 'c-readtable))

;;; reader

(defun cstr (str)
  (with-input-from-string (s str)
    (let ((*compiler-state* (make-compiler-state))
          (*readtable*      c-readtable))
      (cons 'progn (loop for it = (read s nil 'eof)
                         while (not (eq it 'eof)) collect it)))))

(defun %load-c-file (*c-file* *compiler-state*)
  (let ((*readtable*   c-readtable)
        (*line-number* 1))
    (load *c-file*)))

(defun load-c-file (file &key include-paths)
  (%load-c-file file (make-compiler-state :include-paths include-paths)))
