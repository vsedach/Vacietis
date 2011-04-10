(in-package #:vacietis.reader)
(in-readtable vacietis)

(declaim (optimize (debug 3)))

;;; set up lists of constants for reader to use (in separate package to avoid typing package prefix)

(in-package #:vacietis.c)

(cl:defparameter vacietis.reader::*ops*
  '(= += -= *= /= %= <<= >>= &= ^= |\|=| ? |:| |\|\|| && |\|| ^ & == != < > <= >= << >> ++ -- + - * / ! ~ -> |.|))

(cl:defparameter vacietis.reader::*prefix-ops* '(++ -- ~ ! - + & *))

(cl:defparameter vacietis.reader::*unambiguous-prefix-ops* '(++ -- ! ~))

(cl:defparameter vacietis.reader::*assignment-ops* '(= += -= *= /= %= <<= >>= &= ^= |\|=|))

(cl:defparameter vacietis.reader::*binary-ops-table*
  '(|\|\|| ; or
    &&     ; and
    |\||   ; logior
    ^      ; logxor
    &      ; logand
    == !=
    < > <= >=
    << >>  ; ash
    + -
    * / &))

(cl:defparameter vacietis.reader::*basic-c-types* '(int static void const signed unsigned short long float double char extern))

(cl:in-package #:vacietis.reader)

;;; this makes things a lot less hairy

(defvar %in)

;;; error reporting

(define-condition c-reader-error (reader-error) ;; SBCL hates simple-conditions?
  ((line-number :reader line-number :initform *line-number*)
   (msg         :reader msg         :initarg  :msg))
  (:report (lambda (condition stream)
             (write-string (msg condition) stream))))

(defun read-error (msg &rest args)
  (error (make-condition 'c-reader-error
                         :stream %in
                         :msg (format nil "Error reading from C stream at line ~a: ~?"
                                      *line-number* msg args))))

;;; basic stream stuff

(defvar *line-number* 0)

(defun c-read-char ()
  (let ((c (read-char %in nil 'end)))
    (when (eql c #\Newline)
      (incf *line-number*))
    c))

(defun c-unread-char (c)
  (when (eql c #\Newline)
    (decf *line-number*))
  (unread-char c %in))

(defun c-read-line ()
  (incf *line-number*)
  (read-line %in))

(defmacro loop-reading (&body body)
  `(loop with c do (setf c (c-read-char))
        ,@body))

(defun whitespace? (c)
  (or (char= c #\Space) (char= c #\Tab) (char= c #\Newline)))

(defun next-char (&optional (eof-error? t))
  (loop-reading
     while (if (eq 'end c)
               (when eof-error?
                 (read-error "Unexpected end of file"))
               (whitespace? c))
     finally (return c)))

(defun make-string-buffer ()
  (make-array '(3) :adjustable t :fill-pointer 0 :element-type 'character))

(defun slurp-while (predicate)
  (let ((string-buffer (make-string-buffer)))
    (loop-reading
       while (and (not (eq c 'end)) (funcall predicate c))
       do (vector-push-extend c string-buffer)
       finally (unless (eq c 'end) (c-unread-char c)))
    string-buffer))

;;; numbers

(defun read-octal ()
  (parse-integer (slurp-while (lambda (c) (char<= #\0 c #\7))) :radix 8))

(defun read-hex ()
  (parse-integer (slurp-while (lambda (c) (or (char<= #\0 c #\9) (char-not-greaterp #\A c #\F)))) :radix 16))

(defun read-float (prefix separator)
  (let ((*readtable* (find-readtable :common-lisp)))
    (read-from-string
     (format nil "~d~a~a" prefix separator
             (slurp-while (lambda (c) (find c "0123456789+-eE" :test #'char=)))))))

(defun read-decimal (c0) ;; c0 must be #\1 to #\9
  (labels ((digit-value (c) (- (char-code c) 48)))
    (let ((value (digit-value c0)))
      (loop-reading
           (cond ((eq c 'end) (return value))
                 ((char<= #\0 c #\9) (setf value (+ (* 10 value) (digit-value c))))
                 ((or (char-equal c #\E) (char= c #\.)) (return (read-float value c)))
                 (t (c-unread-char c) (return value)))))))

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

(defun read-character-constant ()
  (prog1 (read-char-literal (c-read-char))
    (unless (char= (c-read-char) #\')
      (read-error "Junk in character constant"))))

(defun read-c-string (c1)
  (declare (ignore c1))
  (let ((string (make-string-buffer)))
    (loop-reading
       (if (char= c #\") ;; c requires concatenation of adjacent string literals, retardo
           (progn (setf c (next-char nil))
                  (unless (eql c #\")
                    (unless (eq c 'end) (c-unread-char c))
                    (return string)))
           (vector-push-extend (read-char-literal c) string)))))

;;; preprocessor

(defvar *in-preprocessor-p* nil)
(defvar *in-preprocessor-conditional-p* nil)
(defvar *preprocessor-eval-p* t)

(defun preprocessor-if-test (test-str)
  (not (eql 0 (eval test-str)))) ;; HALP

;; (defun read-c-macro (stream)
;;   (let ((pp-directive (read stream t nil t)))
;;     (case pp-directive
;;       (include
;;        (next-char stream)
;;        (let ((delimiter (case (c-read-char stream)
;;                           (#\" #\") (#\< #\>)
;;                           (otherwise (read-error stream "Error reading include path: ~A" (c-read-line stream))))))
;;          (let ((file (slurp-while stream (lambda (c) (char/= c delimiter)))))
;;            (when *preprocessor-eval-p*
;;              (c-read (c-file-stream file) t)))))
;;       (if
;;        (let* ((*in-preprocessor-conditional-p* t)
;;               (test (c-read-line stream))
;;               (*preprocessor-eval-p* (when *preprocessor-eval-p* (preprocessor-if-test test))))
;;          (read-c-macro stream)))
;;       (endif
;;        (when (not *in-preprocessor-conditional-p*)
;;          (read-error stream "Misplaced #endif")))
;;       (otherwise
;;        (read-error stream "Unknown preprocessor directive #~A" pp-directive)))))

;;; reader

;; (defun c-read (stream &optional recursive-p)
;;   (let ((*readtable* (find-readtable 'c-readtable))
;;         (*in-preprocessor-p* nil)
;;         (*in-preprocessor-conditional-p* nil)
;;         (*preprocessor-eval-p* t)
;;         (c (next-char stream)))
;;     (read stream )))

(defun cstr (str) ;; for development
  (let ((*readtable* (find-readtable 'c-readtable)))
    (read-from-string str)))

;;; infix

(defun parse-nary (args)
  (flet ((split-recurse (x)
           (list (elt args x) (parse-infix (subseq args 0 x)) (parse-infix (subseq args (1+ x))))))
    (acond ((and (consp (car args)) (consp (caar args)) (some #'c-type? (caar args)))
            (parse-nary (cdr args))) ;; ignore casts
           ((position-if (lambda (x) (member x *assignment-ops*)) args)
            (split-recurse it))
           ((position 'vacietis.c:? args)
            (let ((?pos it))
              (append (list 'vacietis.c:if (parse-infix (subseq args 0 ?pos)))
                      (aif (position 'vacietis.c:|:| args)
                           (list (parse-infix (subseq args (1+ ?pos) it)) (parse-infix (subseq args (1+ it))))
                           (read-error "Error parsing ?: trinary operator in: ~A" args)))))
           ((loop for op in *binary-ops-table* thereis (position op args :start 1))
            (split-recurse it))
           ((find (first args) *prefix-ops*)
            (if (= 3 (length args))
                (parse-unary (first args) (parse-unary (second args) (third args)))
                (read-error "Error parsing expression: ~A" args)))
           (t args)))) ;; assume arglist

(defun parse-unary (a b)
  (acond ((find a *unambiguous-prefix-ops*)
          (list it b))
         ((and (listp a) (listp (car a)) (some #'c-type? (car a)))
          (parse-infix b)) ;; looks like a cast, ignore it
         (t (case a
              (vacietis.c:* `(vacietis.c:deref* ,b))
              (vacietis.c:& `(vacietis.c:mkptr& ,b))
              (t (case b
                   (vacietis.c:++ `(vacietis.c:post++ ,a))
                   (vacietis.c:-- `(vacietis.c:post-- ,a))
                   (t  `(,a ,@(mapcar #'parse-infix b))))))))) ;; assume funcall for now

(defun parse-infix (args)
  (if (consp args)
      (case (length args)
        (1 (parse-infix (car args)))
        (2 (parse-unary (first args) (second args)))
        (t (parse-nary args)))
      args))

;;; statements

(defun read-c-block (c)
  (if (eql c #\{)
      (cons 'tagbody
            (loop with c do (setf c (next-char))
                  until (eql c #\}) append (reverse
                                            (multiple-value-list
                                             (read-c-statement c)))))
      (read-error "Expected opening brace '{' but found '~A'" c)))

(defun c-type? (identifier)
  ;; and also do checks for struct, union, enum and typedef types
  (member identifier *basic-c-types*))

(defun next-exp ()
  (read-c-exp (next-char)))

(defun read-control-flow-statement (statement)
  (macrolet ((control-flow-case (statement &body cases)
               `(case ,statement
                  ,@(loop for (symbol . body) in cases collect
                         `(,symbol (list ',symbol ,@body))))))
    (flet ((read-block-or-statement ()
             (let ((next-char (next-char)))
               (if (eql next-char #\{)
                   (read-c-block next-char)
                   (read-c-statement next-char)))))
     (control-flow-case statement
       (vacietis.c:if (parse-infix (next-exp))
                      (read-block-or-statement))
       (vacietis.c:return (read-c-statement (next-char)))
       (vacietis.c:while (parse-infix (next-exp))
                         (read-block-or-statement))))))

(defun read-comma-separated-list (open-delimiter)
  (let ((close-delimiter (ecase open-delimiter (#\( #\)) (#\{ #\})))
        (acc ()))
    (flet ((gather-acc ()
             (prog1 (reverse acc) (setf acc ()))))
      (append (loop with c do (setf c (next-char))
                    until (eql c close-delimiter)
                    if (eql #\, c) collect (gather-acc)
                    else do (push (read-c-exp c) acc))
              (awhen (gather-acc) (list it))))))

(defvar *variable-declarations*)

(defun read-function (name)
  `(defun ,name ,(remove-if #'c-type? ;; do the right thing with type declarations
                            (reduce #'append
                                    (read-comma-separated-list (next-char))))
     ,(let* ((*variable-declarations* ())
             (body (read-c-block (next-char))))
        `(let ,*variable-declarations*
           ,body))))

(defun read-variable-declaration (first-name)
  ;; have to deal with array declarations like *foo_bar[baz]
  (labels ((declare-var (name)
             (if (boundp '*variable-declarations*)
                 (progn (push (list name 0) *variable-declarations*)
                        `(setf ,name 0))
                 `(defvar ,name)))
           (read-decl (declared)
             (acase (next-char)
               (#\; declared)
               (#\, (read-decl declared))
               (t   (read-decl (cons (declare-var (read-c-exp it)) declared))))))
    (cons 'progn (read-decl (list (declare-var first-name))))))

(defun read-declaration (token)
  (when (c-type? token)
    (let ((name (loop with x do (setf x (next-exp))
                      while (or (eql 'vacietis.c::* x) (c-type? x))
                      finally (return x)))) ;; throw away type info
      (if (eql #\( (peek-char t %in))
          (read-function name)
          (read-variable-declaration name)))))

(defun read-labeled-statement (token)
  (when (eql #\: (peek-char t %in))
    (next-char)
    (values (read-c-statement (next-char)) token)))

(defun read-c-statement (c)
  (let ((next-token (read-c-exp c)))
    ;; m-v-b is here because OR doesn't return multiple values except for last expression
    (multiple-value-bind (statement label) (read-labeled-statement next-token)
      (if statement
          (values statement label)
          (or (read-declaration next-token)
              (read-control-flow-statement next-token)
              (parse-infix (cons next-token
                                 (loop with c do (setf c (next-char))
                                    until (eql c #\;) collect (read-c-exp c)))))))))

(defun read-c-identifier (c)
  ;; assume inverted readtable (need to fix for case-preserving lisps)
  (let* ((raw-name (concatenate 'string (string c) (slurp-while (lambda (c) (or (eql c #\_) (alphanumericp c))))))
         (raw-name-alphas (remove-if-not #'alpha-char-p raw-name))
         (identifier-name (format nil (cond ((every #'upper-case-p raw-name-alphas) "~(~A~)")
                                            ((every #'lower-case-p raw-name-alphas) "~:@(~A~)")
                                            (t "~A"))
                                  raw-name)))
    (or (find-symbol identifier-name '#:vacietis.c) (intern identifier-name))))



;; this would be really simple if streams could unread more than one char
;; also if CLISP didn't have bugs w/unread-char after peek and near EOF
(defun match-longest-op (one)
  (flet ((seq-matches (&rest chars)
           (find (make-array (length chars) :element-type 'character :initial-contents chars)
                 *ops* :test #'string= :key #'symbol-name)))
    (let* ((two       (c-read-char))
           (two-match (seq-matches one two)))
      (if two-match
          (let ((three-match (seq-matches one two (peek-char nil %in))))
            (if three-match
                (progn (c-read-char) three-match)
                two-match))
          (progn (c-unread-char two)
                 (seq-matches one))))))

(defun read-c-exp (c)
  (or (match-longest-op c)
      (cond ((digit-char-p c) (read-c-number c))
            ((or (eql c #\_) (alpha-char-p c)) (read-c-identifier c))
            (t
             (case c
               ;; (#\# (let ((*in-preprocessor-p* t)) ;; preprocessor
               ;;        (read-c-macro stream)))
               ;; (#\/ (c-read-char stream) ;; comment
               ;;      (case (c-read-char stream)
               ;;        (#\/ (c-read-line stream))
               ;;        (#\* (slurp-while stream
               ;;                          (let ((previous-char (code-char 0)))
               ;;                            (lambda (c)
               ;;                              (prog1 (not (and (char= previous-char #\*)
               ;;                                               (char= c #\/)))
               ;;                                (setf previous-char c)))))
               ;;             (c-read-char stream))
               ;;        (otherwise (read-error stream "Expected comment"))))
               (#\" (read-c-string c))
               (#\( (read-comma-separated-list #\())
               (#\[ (list 'aref
                          (parse-infix (loop with c do (setf c (next-char))
                                          until (eql c #\]) collect (read-c-exp c))))))))))

;;; readtable

(defun read-c-toplevel (stream c)
  (let ((%in stream)
        (*line-number* 0))
    (read-c-statement c)))

(macrolet
    ((def-c-readtable ()
       `(defreadtable c-readtable
         (:case :invert)

         ;; unary and prefix operators
         ,@(loop for i in '(#\+ #\- #\~ #\! #\( #\& #\*) collect `(:macro-char ,i 'read-c-toplevel nil))

;         (:macro-char #\# 'read-c-macro nil)

         ;; numbers (should this be here?)
         ,@(loop for i from 0 upto 9 collect `(:macro-char ,(digit-char i) 'read-c-toplevel nil))

         ;; identifiers
         (:macro-char #\_ 'read-c-toplevel nil)
         ,@(loop for i from (char-code #\a) upto (char-code #\z) collect `(:macro-char ,(code-char i) 'read-c-toplevel nil))
         ,@(loop for i from (char-code #\A) upto (char-code #\Z) collect `(:macro-char ,(code-char i) 'read-c-toplevel nil))
         )))
  (def-c-readtable))
