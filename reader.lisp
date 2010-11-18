(in-package #:reader.vacietis)

;;; basic stream stuff

(defvar *line-number*)
(defvar *column*)

(defun c-read-char (stream)
  (let ((c (read-char stream)))
    (if (eql c #\Newline)
        (progn (incf *line-number*) (setf *column* 0))
        (incf *column*))
    c))

(defun c-unread-char (c stream)
  (when (eql c #\Newline) ;; what do we do about column counts?
    (decf *line-number*))
  (unread-char c stream))

(defmacro loop-read (stream &body body)
  `(loop with c do (setf c (c-read-char ,stream))
        ,@body))

(defun skip-whitespace (stream)
  (loop-read stream
     while (member c '(#\Space #\Tab #\Newline))
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

(defun read-decimal (stream) ;; sooo broken
  (labels ((digit-value (c) (- (char-code c) 48))
           (read-float-exponent (value)
             (let ((exp 0))
               (loop-read stream
                    (unless (char<= #\0 c #\9)
                      (c-unread-char c stream)
                      (return (* value (expt 10 exp))))
                    (setf exp (+ (digit-value c) (* 10 exp))))))
           (read-float-fraction (value)
             (let ((mult 0.1))
               (loop-read stream
                    (unless (char<= #\0 c #\9)
                      (return (if (char-equal c #\E)
                                  (read-float-exponent value)
                                  (progn (c-unread-char c stream) value))))
                    (setf value (+ value (* mult (digit-value c)))
                          mult (/ mult 10.0))))))
    (let ((value 0))
      (loop-read stream
           (cond ((char<= #\0 c #\9) (setf value (+ (* 10 value) (digit-value c))))
                 ((char= c #\.) (return (read-float-fraction value)))
                 ((char-equal c #\E) (return (read-float-exponent value)))
                 (t (return (progn (c-unread-char c stream) value))))))))

(defun read-c-number (stream c) ;; todo: need to discard suffixes
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
