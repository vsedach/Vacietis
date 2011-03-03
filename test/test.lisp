;;; The contents of this file are released into the public domain.

(in-package #:vacietis.test)

(def-suite vacietis-reader)

(defun run-tests ()
  (format t "Running reader tests:~&")
  (run! 'vacietis-reader))

(defun case-sensitive-equalp (x y)
  (if (and (stringp x) (stringp y))
      (string= x y)
      (equal x y)))

(defmacro reader-test (name input s-exp)
  `(test ,testname ()
     (is (case-sensitive-equalp
          ',s-exp
          (let ((*readtable* (find-readtable 'c-readtable)))
            (read-from-string ,input))))))
