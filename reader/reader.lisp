(in-package #:vacietis.reader)
(in-readtable vacietis)

(declaim (optimize (debug 3)))

(in-package #:vacietis.c)

(cl:defparameter vacietis.reader::*ops*
  #(= += -= *= /= %= <<= >>= &= ^= |\|=| ? |:| |\|\|| && |\|| ^ & == != < > <= >= << >> ++ -- + - * / % ! ~ -> |.| |,|))

(cl:defparameter vacietis.reader::*possible-prefix-ops*
  #(! ~ sizeof - + & * ++ --))

(cl:defparameter vacietis.reader::*ambiguous-ops*
  #(- + & *))

(cl:defparameter vacietis.reader::*assignment-ops*
  #(= += -= *= /= %= <<= >>= &= ^= |\|=|))

(cl:defparameter vacietis.reader::*binary-ops-table*
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

(cl:in-package #:vacietis.reader)

;;; this makes things a lot less hairy

(defvar %in)
(defvar *c-file* nil)
(defvar *line-number*  nil)

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
             ((#\Space #\Newline #\Tab) t))
     finally (return c)))

(defun make-buffer (&optional (element-type t))
  (make-array 10 :adjustable t :fill-pointer 0 :element-type element-type))

(defun slurp-while (predicate)
  (let ((string-buffer (make-buffer 'character)))
    (loop-reading
       while (and c (funcall predicate c))
       do (vector-push-extend c string-buffer)
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
                 (t
                  (c-unread-char c)
                  (return value)))))))

(defun read-c-number (c)
  (prog1 (if (char= c #\0)
             (let ((next (peek-char nil %in)))
               (if (digit-char-p next 8)
                   (read-octal)
                   (case next
                     ((#\X #\x) (c-read-char) (read-hex))
                     (#\.       (c-read-char) (read-float 0 #\.))
                     (otherwise 0))))
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

(defun pp-read-line ()
  (let (comment-follows?)
   (prog1
       (slurp-while (lambda (c)
                      (case c
                        (#\Newline)
                        (#\/ (if (find (peek-char nil %in nil nil) "/*")
                                 (progn (setf comment-follows? t) nil)
                                 t))
                        (t t))))
     (c-read-char)
     (when comment-follows?
       (%maybe-read-comment)))))

(defmacro lookup-define ()
  `(gethash (read-c-identifier (next-char)) *preprocessor-state*))

(defun starts-with? (str x)
  (string= str x :end1 (min (length str) (length x))))

(defun preprocessor-skip-branch ()
  (let ((if-nest-depth 1))
    (loop for line = (pp-read-line) do
         (cond ((starts-with? line "#if")
                (incf if-nest-depth))
               ((and (starts-with? line "#endif")
                     (= 0 (decf if-nest-depth)))
                (pop preprocessor-if-stack)
                (return))
               ((and (starts-with? line "#elif")
                     (= 1 if-nest-depth))
                (case (car preprocessor-if-stack)
                  (if (when (preprocessor-test (pp-read-line))
                        (setf (car preprocessor-if-stack) 'elif)
                        (return)))
                  (elif nil)
                  (else (read-error "Misplaced #elif"))))))))

(defun preprocessor-test (line)
  (let ((exp (with-input-from-string (%in line)
               (read-infix-exp (read-c-exp (next-char))))))
    (not (eql 0 (eval `(symbol-macrolet ,(let ((x))
                                              (maphash (lambda (k v)
                                                         (push (list k v) x))
                                                       *preprocessor-state*)
                                              x)
                         ,exp))))))

(defun fill-in-template (args template subs)
  (ppcre:regex-replace-all
   (format nil "([^a-zA-Z])?(~{~a~^|~})([^a-zA-Z0-9])?" args)
   template
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

(defun read-c-macro (%in sharp)
  (declare (ignore sharp))
  ;; preprocessor directives need to be read in a separate namespace
  (let ((pp-directive (read-c-identifier (next-char))))
    (case pp-directive
      (vacietis.c:define
       (setf (lookup-define)
             (if (eql #\( (peek-char t %in))
                 (let ((args     (c-read-delimited-strings t))
                       (template (string-trim '(#\Space #\Tab) (pp-read-line))))
                   (lambda (substitutions)
                     (if args
                         (fill-in-template args template substitutions)
                         template)))
                 (pp-read-line))))
      (vacietis.c:undef
       (remhash (read-c-identifier (next-char)) *preprocessor-state*)
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
                           *preprocessor-state*)
             (include-libc-file include-file))))
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
  nil)

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
                 (match-binary-ops (table)
                   (position-if (lambda (x) (find x table))
                                exp :start (1+ start) :end (1- end)))
                 (parse-binary (i &optional op)
                   (list (or op (aref exp i))
                         (parse-infix exp start  i)
                         (parse-infix exp (1+ i) end))))
          ;; in order of weakest to strongest precedence
          ;; comma
          (awhen (match-binary-ops '(vacietis.c:|,|))
            (return (parse-binary it 'progn)))
          ;; assignment
          (awhen (match-binary-ops *assignment-ops*)
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
                 (let ((prev (aref exp (1- it))))
                   (unless (and (find (aref exp it) *ambiguous-ops*)
                                (or (cast? prev)
                                    (find prev *possible-prefix-ops*)))
                     (return-from parse-infix (parse-binary it))))))
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
          (loop for i from (1+ start) below end for x = (aref exp i) do
               (cond
                 ((find x #(vacietis.c:|.| vacietis.c:->))
                  (return-from parse-infix (parse-binary i)))
                 ((listp x) ;; aref
                  (return-from parse-infix
                    (if (eq (car x) 'vacietis.c:[])
                        `(vacietis.c:[] ,(parse-infix exp start i)
                                        ,(parse-infix (second x)))
                        (read-error "Unexpected list when parsing ~A" exp))))
                 ((vectorp x) ;; funcall
                  (return-from parse-infix
                    (cons (parse-infix exp start i)
                          (loop with xstart = 0
                             for next = (position 'vacietis.c:|,| x :start xstart)
                             when (< 0 (length x))
                             collect (parse-infix x xstart (or next (length x)))
                             while next do (setf xstart (1+ next))))))))
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

(defun type-qualifier? (x)
  (member x *type-qualifiers*))

(defun basic-type? (x)
  (member x *basic-c-types*))

(defun c-type? (identifier)
  ;; and also do checks for struct, union, enum and typedef types
  (or (type-qualifier? identifier)
      (basic-type? identifier)
      (member identifier '(vacietis.c:struct))))

(defun next-exp ()
  (read-c-exp (next-char)))

(defvar *variable-declarations*)

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
          (vacietis.c:return
            `(return ,(or (read-c-statement (next-char)) 0)))
          (vacietis.c:while
            `(vacietis.c:for (nil nil ,(parse-infix (next-exp)) nil)
               ,(read-block-or-statement)))
          (vacietis.c:do
           (let ((body (read-block-or-statement)))
             (if (eql (next-exp) 'vacietis.c:while)
                 (prog1
                     `(vacietis.c:do ,body ,(parse-infix (next-exp)))
                   (read-c-statement (next-char))) ;; semicolon
                 (read-error "No 'while' following a 'do'"))))
          (vacietis.c:for
            `(vacietis.c:for
                 ,(let* ((*variable-sizes*        (cons (make-hash-table)
                                                        *variable-sizes*))
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

(defun read-function (name)
  (let ((*variable-sizes* (cons (make-hash-table) *variable-sizes*)))
   `(defun ,name
        ,(loop for xs across (c-read-delimited-list (next-char) #\,) append
              (loop for x across xs
                    unless (or (c-type? x) (eql 'vacietis.c:* x))
                    collect x))
      ,(let* ((*variable-declarations* ())
              (body (read-c-block (next-char))))
        `(prog* ,*variable-declarations* ,@body)))))

;; fixme: shit code
(defun process-variable-declaration (spec type)
  (let (name (preallocated-value 0) initial-value)
    (labels ((parse-declaration (x)
               (if (symbolp x)
                   (setf name x)
                   (progn (case (car x)
                            (vacietis.c:= (setf initial-value (third x)))
                            (vacietis.c:[]
                             (awhen (third x)
                               (setf preallocated-value
                                     `(vacietis:allocate-memory ,it)))))
                          (parse-declaration (second x))))))
      (parse-declaration spec)
      ;; this is totalle broken
      (setf (gethash name (car *variable-sizes*))
            (cond ((and (vectorp initial-value)
                        (eq 'literal (car initial-value)))
                   (length (elt initial-value 1)))
                  ((consp preallocated-value)
                   (second preallocated-value))
                  ((consp type) ;; assume struct
                   (let ((type-size (size-of (second type))))
                     (setf preallocated-value
                           `(vacietis:allocate-memory ,type-size))
                     type-size))
                  (t 1)))
      (if (boundp '*variable-declarations*)
          (progn (push (list name preallocated-value) *variable-declarations*)
                 (when initial-value `((vacietis.c:= ,name ,initial-value))))
          `((defparameter ,name ,(or initial-value preallocated-value)))))))

(defun read-variable-declaration (first-name type)
  (let* ((decls (c-read-delimited-list #\; #\,))
         (first (make-array (1+ (length (aref decls 0))))))
    (replace first (aref decls 0) :start1 1)
    (setf (aref first 0) first-name
          (aref decls 0) first)
    (aif (loop for x across decls appending
              (process-variable-declaration (parse-infix x) type))
         (cons 'progn it)
         t)))

(defun read-struct-slots (sc)
  (case sc
    (#\{ (loop for c = (next-char) until (eql c #\}) append
              (loop for x = (read-c-exp c) then (next-exp)
                    while (or (eql 'vacietis.c:* x) (c-type? x))
                    finally (return (when x (list x))))))
    (#\; nil) ;; forward declaration
    (otherwise (read-error "Expected opening brace '{' but found '~A'" sc))))

(defun read-struct (name)
  (let* ((slots        (read-struct-slots (next-char)))
         (declaration `(vacietis::c-struct ,name ,@slots)))
    (when slots
      (setf (gethash name *c-structs*) slots)
      (if (eql #\; (peek-char t %in))
          (next-char)
          (return-from read-struct
            `(progn ,declaration
                    ,(read-variable-declaration (next-exp) `(struct ,name))))))
    declaration))

(defun read-declaration (token)
  (when (c-type? token)
    (let ((type     (unless (type-qualifier? token) token))
          (pointer? nil)
          (name     nil))
      (loop for x = (next-exp) do
           (cond ((eql 'vacietis.c:* x) (setf pointer? t))
                 ((basic-type? x)       (setf type x))
                 ((type-qualifier? x)   nil)
                 (t                     (setf name x) (return))))
      (cond ((eql #\( (peek-char t %in))
             (read-function name))
            ((eql type 'vacietis.c:struct)
             (read-struct name))
            (t
             (read-variable-declaration name (unless pointer? type)))))))

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
  (unless (eql #\; c)
    (%read-c-statement (read-c-exp c))))

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

(defun read-array-literal ()
  (let ((content (map 'list #'parse-infix (c-read-delimited-list #\{ #\,))))
    `(vacietis::make-memptr
      :mem (make-array ,(length content) :initial-contents (list ,@content)))))

(defun read-c-exp (c)
  (or (match-longest-op c)
      (cond ((digit-char-p c) (read-c-number c))
            ((or (eql c #\_) (alpha-char-p c))
             (let ((symbol (read-c-identifier c)))
               (aif (gethash symbol *preprocessor-state*)
                    (progn (setf %in
                                 (make-concatenated-stream
                                  (make-string-input-stream
                                   (etypecase it
                                     (string it)
                                     (function
                                      (funcall it (c-read-delimited-strings)))))
                                  %in))
                           (read-c-exp (next-char)))
                    symbol)))
            (t
             (case c
               (#\" (read-c-string %in c))
               (#\' (read-character-constant %in c))
               (#\( (read-exps-until (lambda (c) (eql #\) c))))
               (#\{ (read-array-literal)) ;; decl only
               (#\[ (list 'vacietis.c:[]
                          (read-exps-until (lambda (c) (eql #\] c))))))))))

;;; readtable

(defun read-c-toplevel (%in c)
  (read-c-statement c))

(macrolet
    ((def-c-readtable ()
       `(defreadtable c-readtable
         (:case :invert)

         ;; unary and prefix operators
         ,@(loop for i in '(#\+ #\- #\~ #\! #\( #\& #\*)
              collect `(:macro-char ,i 'read-c-toplevel nil))

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
    (let ((*preprocessor-state* (make-pp-state))
          (*readtable*          c-readtable))
      (cons 'progn (loop for it = (read s nil 'eof)
                         while (not (eq it 'eof)) collect it)))))

(defun %load-c-file (*c-file* *preprocessor-state*)
  (let ((*readtable*   c-readtable)
        (*line-number* 1))
    (load *c-file*)))

(defun vacietis:load-c-file (file)
  (%load-c-file file (make-pp-state)))
