;;; The contents of this file are released into the public domain.

(in-package #:vacietis.test)
(in-readtable vacietis)

(def-suite vacietis-reader)
(def-suite basic-tests)
(def-suite program-tests)

(defun run-tests ()
  (format t "Running reader tests:~&")
  (run! 'vacietis-reader)
  (format t "Running basic tests:~&")
  (run! 'basic-tests)
  (format t "Running program tests:~&")
  (run! 'program-tests))

(defun c-equal (C CL)
  (let ((CL (if (stringp CL) (string-to-char* CL) CL)))
    (if (or (atom C) (atom CL))
        (equalp C CL)
        (and (c-equal (car C) (car CL)) (c-equal (cdr C) (cdr CL))))))

(defmacro reader-test (name input &rest s-exps)
  `(test ,name
     (is (c-equal
          (vacietis.reader::cstr ,input)
          '(progn ,@s-exps)))))

(defmacro eval-test (name input result)
  `(test ,name
     (is (equalp ,result (eval (vacietis.reader::cstr ,input))))))

(defmacro program-test (name &key return-code input output)
  `(test ,name
     (let ((test-package (make-package
                          (gensym (format nil "VACIETIS.TEST.~A" ',name))
                          :use '(#:vacietis.c))))
       (unwind-protect
            (progn
              (let ((*package* test-package))
                (load-c-file
                 (merge-pathnames
                  (format nil "programs/~(~A~)/main.c" ',name)
                  (directory-namestring #.*compile-file-truename*))))
              (let* ((test-output-stream (when ,output
                                           (make-string-output-stream)))
                     (result (run-c-program
                              test-package
                              :stdin (when ,input
                                       (make-string-input-stream ,input))
                              :stdout test-output-stream)))
                (when ,return-code
                  (is (equal ,return-code result)))
                (when ,output
                  (is (equal ,output (get-output-stream-string
                                      test-output-stream))))))
         (delete-package test-package)))))
