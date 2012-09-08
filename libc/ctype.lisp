(in-package #:vacietis.libc.ctype.h)
(in-readtable vacietis:vacietis)

(defmacro chartest (cname exp)
  `(defun/1 ,cname (code)
     (if (<= 0 code)
         (let ((c (code-char code)))
           (declare (ignorable c))
           (if ,exp 1 0))
         0)))

(defun space? (c)
  (member c '(#\Space #\Newline #\Return #\Tab))) ;; vertical tab

(chartest isspace (space? c))
(chartest isalnum (alphanumericp c))
(chartest isalpha (alpha-char-p c))
(chartest iscntrl (or (<= 0 code #x1F) (eql code 127))) ;; assume ASCII ctrl codes
(chartest isdigit (digit-char-p c))
(chartest isgraph (and (graphic-char-p c) (not (space? c))))
(chartest islower (lower-case-p c))
(chartest isprint (graphic-char-p c))
(chartest ispunct (and (graphic-char-p c)
                       (not (space? c))
                       (not (alphanumericp c))))
(chartest isupper (upper-case-p c))
(chartest isxdigit (digit-char-p c 16))

(defun/1 toupper (c)
  (char-upcase (code-char c)))

(defun/1 tolower (c)
  (char-downcase (code-char c)))
