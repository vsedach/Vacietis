(in-package #:vacieits.ctype)

(defmacro chartest (cname exp)
  `(defun ,cname (code)
     (let ((c (code-char code)))
       (if ,exp 1 0))))

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

(defun toupper (c)
  (char-upcase (code-char c)))

(defun tolower (c)
  (char-downcase (code-char c)))
