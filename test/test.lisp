;;; The contents of this file are released into the public domain.

(in-package #:vacietis.test)

(def-suite vacietis-reader)
(def-suite basic-tests)

(defun run-tests ()
  (format t "Running reader tests:~&")
  (run! 'vacietis-reader)
  (format t "Running basic tests:~&")
  (run! 'basic-tests))

(defun c-equal (C CL)
  (let ((CL (if (stringp CL) (string-to-char* CL) CL)))
    (if (or (atom C) (atom CL))
        (equalp C CL)
        (and (c-equal (car C) (car CL)) (c-equal (cdr C) (cdr CL))))))

(defmacro reader-test (name input &rest s-exps)
  `(test ,name ()
         (is (c-equal
              (vacietis.reader::cstr ,input)
              '(progn ,@s-exps)))))

(defmacro eval-test (name input result)
  `(test ,name ()
         (is (equalp ,result (eval (vacietis.reader::cstr ,input))))))
