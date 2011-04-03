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

(defmacro reader-test (name input s-exp)
  `(test ,name ()
     (is (case-sensitive-equalp
          ',s-exp
          (let ((*readtable* (find-readtable 'c-readtable)))
            (read-from-string ,input))))))

(defmacro eval-test (name input result)
  `(test ,name ()
     (is (equal ,result
                (let ((*readtable* (find-readtable 'c-readtable)))
                  (with-input-from-string (s ,input)
                   (eval (cons 'progn
                               (loop with it do (setf it (read s nil))
                                  while it collect it)))))))))
