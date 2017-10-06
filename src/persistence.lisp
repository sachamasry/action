(in-package :cl-user)
(defpackage :action/persistence
  (:use :cl)
  (:import-from :action/filesystem-interface
                #:split-path
                #:construct-destination-path
                #:construct-file-name
                #:copy-file
                #:rm-file)
  (:export #:write-sexp-to-file
           #:read-sexp-from-file
           #:create-file-snapshot))
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

(defun create-file-snapshot (file &key (delimiter #\-))
  (when (and file
             (probe-file file))
    (let ((timestamp
            (local-time:format-timestring
             nil (local-time:now)
             :format '((:year 4) #\-
                       (:month 2) #\-
                       (:day 2) #\T
                       (:hour 2) #\:
                       (:min 2) #\: 
                       (:sec 2) #\.
                       (:usec 6)))))
      (multiple-value-bind (directory full-file-name file-name file-type)
          (split-path file)
        (let ((dest-file
                (construct-destination-path
                 :source-path directory
                 :destination-file-name 
                 (construct-file-name "" "-"
                                      file-name "snapshot" timestamp)
                 :destination-file-extension file-type)))
          (rm-file dest-file)
          (and 
           (copy-file file dest-file)
           (probe-file dest-file)))))))
