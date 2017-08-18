(in-package :cl-user)
(defpackage :action/persistence
  (:use :cl)
  (:export #:write-sexp-to-file
           #:read-sexp-from-file))
(in-package :action/persistence)

(defvar *keyword-package* (find-package :keyword))

(defun write-sexp-to-file (filename sexp
                           &key (exists-action :supersede))
  (with-open-file (file filename :direction :output
                                 :if-exists exists-action
                                 :if-does-not-exist :create)
    (let ((*package *keyword-package*))
      (with-standard-io-syntax
        (let ((*print-circle* t))
          (print sexp file))))))

(defvar *eof-value* (gensym))

(defun read-sexp-from-file (filename)
  (with-open-file (file filename :direction :input)
    (with-standard-io-syntax
      (let ((*read-eval* nil))
        (prog1 (loop as sexp = (read file nil *eof-value*)
                     until (eq *eof-value* sexp)
                     collect sexp))))))
