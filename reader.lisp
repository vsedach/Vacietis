(in-package #:reader.vacietis)

;;; basic stream stuff

(defvar *line-number* 0)
(defvar *column* 0)

(defun c-read-char (stream)
  (let ((c (read-char stream nil (code-char 0))))
    (if (eql c #\Newline)
        (progn (incf *line-number*) (setf *column* 0))
        (incf *column*))
    c))

(defun c-unread-char (c stream)
  (when (eql c #\Newline) ;; what do we do about column counts?
    (decf *line-number*))
  (unread-char c stream))

(defun c-read-line (stream)
  (incf *line-number*)
  (read-line stream))

(defmacro loop-read (stream &body body)
  `(loop with c do (setf c (c-read-char ,stream))
        ,@body))

(defun whitespace? (c)
  (or (char= c #\Space) (char= c #\Tab) (char= c #\Newline)))

(defun skip-whitespace (stream)
  (loop-read stream
     while (whitespace? c)
     finally (return c)))

(defun make-peek-buffer ()
  (make-array '(3) :adjustable t :fill-pointer 0 :element-type 'character))

(defun slurp-while (stream predicate)
  (let ((peek-buffer (make-peek-buffer)))
    (loop-read stream
       while (funcall predicate c)
       do (vector-push-extend c peek-buffer)
       finally (c-unread-char c stream))
    peek-buffer))

;;; error reporting

(define-condition c-reader-error (reader-error simple-error)
  ((line-number :reader line-number :initarg :line-number)
   (column :reader column :initarg :column))
  (:default-initargs :line-number *line-number* :column *column*))

(defun read-error (stream msg &rest args)
  (error (make-condition 'c-reader-error
                         :stream stream
                         :format-control (format nil "Error reading from C stream (line ~a, column ~a): ~?"
                                                 *line-number* *column* msg args))))

;;; numbers

(defun read-octal (stream)
  (parse-integer (slurp-while stream (lambda (c) (char<= #\0 c #\7))) :radix 8))

(defun read-hex (stream)
  (parse-integer (slurp-while stream (lambda (c) (or (char<= #\0 c #\9) (char-not-greaterp #\A c #\F)))) :radix 16))

(defun read-float (prefix separator stream)
  (parse-number:parse-number
   (format nil "~d~a~a" prefix separator
           (slurp-while stream (lambda (c) (find c "0123456789+-eE" :test #'char=))))))

(defun read-decimal (stream)
  (labels ((digit-value (c) (- (char-code c) 48)))
    (let ((value 0))
      (loop-read stream
           (cond ((char<= #\0 c #\9) (setf value (+ (* 10 value) (digit-value c))))
                 ((or (char-equal c #\E) (char= c #\.)) (return (read-float value c stream)))
                 (t (return (progn (c-unread-char c stream) value))))))))

(defun read-c-number (stream c)
  (prog1 (if (char= c #\0)
             (case (peek-char nil stream)
               ((#\X #\x) (c-read-char stream) (read-hex stream))
               (#\. (c-unread-char c stream) (read-decimal stream))
               (otherwise (read-octal stream)))
             (progn (c-unread-char c stream) (read-decimal stream)))
    (loop repeat 2 do (when (find (peek-char nil stream) "ulf" :test #'char-equal)
                        (c-read-char stream)))))

;;; string and chars (caller has to remember to discard leading #\L!!!)

(defun read-char-literal (stream c)
  (if (char= c #\\)
      (let ((c (c-read-char stream)))
        (code-char (case c
                     (#\a 7)
                     (#\f 12)
                     (#\n 10)
                     (#\r 13)
                     (#\t 9)
                     (#\v 11)
                     (#\x (read-hex stream))
                     (otherwise (if (char<= #\0 c #\7)
                                    (progn (c-unread-char c stream) (read-octal stream))
                                    (char-code c))))))
      c))

(defun read-character-constant (stream)
  (prog1 (read-char-literal stream (c-read-char stream))
    (unless (char= (c-read-char stream) #\')
      (read-error stream "Junk in character constant"))))

(defun read-c-string (stream)
  (let ((string (make-peek-buffer)))
    (loop-read stream
         (if (char= c #\") ;; c requires concatenation of adjacent string literals, retardo
             (progn (setf c (skip-whitespace stream))
                    (unless (char= c #\")
                      (c-unread-char c stream)
                      (return string)))
             (vector-push-extend (read-char-literal stream c) string)))))

;;; keywords

(defvar *keywords* '("auto" "break" "case" "char" "const" "continue" "default" "do" "double" "else" "enum" "extern" "float" "for" "goto" "if" "inline" "int" "long" "register" "restrict" "return" "short" "signed" "sizeof" "static" "struct" "switch" "typedef" "union" "unsigned" "void" "volatile" "while" "_Bool" "_Complex" "_Imaginary"))

;;; preprocessor

(defvar *in-preprocessor-p* nil)
(defvar *in-preprocessor-conditional-p* nil)
(defvar *preprocessor-eval-p* t)

(defun preprocessor-include (stream)
  (let ((file (c-read-line stream)))
    (when *preprocessor-eval-p*
      (c-read (c-file-stream file) t))))

(defun preprocessor-if-test (test-str)
  (not (eql 0 (eval test-str)))) ;; HALP

(defun preprocessor-if (stream)
  (let* ((*in-preprocessor-conditional-p* t)
         (test (c-read-line stream))
         (*preprocessor-eval-p* (when *preprocessor-eval-p* (preprocessor-if-test test))))
    (c-read-macro stream)))



(defun c-read-macro (stream)
  (let ((operator (slurp-while stream (complement #'whitespace?))))
    (cond ((string= operator "#include")
           (preprocessor-include stream))
          ((string= operator "#if")
           (preprocessor-if stream))
          ((string= operator "#endif")
           (when (not *in-preprocessor-conditional-p*)
             (read-error stream "Misplaced #endif"))))))

;;; reader

(defvar *c-read-accumulator* ())

(defun c-read (stream &optional recursive-p)
  (let ((*in-preprocessor-p* nil)
        (*in-preprocessor-conditional-p* nil)
        (*preprocessor-eval-p* t)
        (*c-read-accumulator* (if recursive-p *c-read-accumulator* ()))
        (c (skip-whitespace stream)))
    (c-unread-char c stream)
    (case c
      (#\# (let ((*in-preprocessor-p* t)) ;; preprocessor
             (c-read-macro stream)))
      (#\/ (c-read-char stream) ;; comment
           (case (c-read-char stream)
             (#\/ (c-read-line stream))
             (#\* (slurp-while stream
                               (let ((previous-char (code-char 0)))
                                 (lambda (c)
                                   (prog1 (not (and (char= previous-char #\*)
                                                    (char= c #\/)))
                                     (setf previous-char c)))))
                  (c-read-char stream))
             (otherwise (read-error stream "Expected comment")))))
    (reverse *c-read-accumulator*)))
