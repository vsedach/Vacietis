;;; The contents of this file are released into the public domain.

(in-package #:vacietis.test)

(def-suite vacietis-reader)
(def-suite basic-tests)

(defun run-tests ()
  (format t "Running reader tests:~&")
  (run! 'vacietis-reader)
  (format t "Running basic tests:~&")
  (run! 'basic-tests))

(defun case-sensitive-equalp (x y)
  (if (and (stringp x) (stringp y))
      (string= x y)
      (equal x y)))

(defmacro reader-test (name input &rest s-exps)
  `(test ,name ()
         (is (case-sensitive-equalp
              '(progn ,@s-exps)
              (vacietis.reader::cstr ,input)))))

(defmacro eval-test (name input result)
  `(test ,name ()
         (is (equalp ,result (eval (vacietis.reader::cstr ,input))))))
