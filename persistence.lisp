(in-package :cl-user)
(defpackage :action/persistence
  (:use :cl)
  (:import-from :fad
                #:directory-exists-p)
  (:import-from :action/filesystem-interface
                #:split-path
                #:construct-destination-path
                #:construct-file-name
                #:copy-file
                #:rm-file)
  (:import-from :local-time
                #:today
                #:now
                #:format-timestring
                #:timestamp-to-universal
                #:universal-to-timestamp)
  (:export #:write-sexp-to-file
           #:read-sexp-from-file
           #:create-file-snapshot
           #:construct-report-file-path))
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
        (loop as sexp = (read file nil *eof-value*)
              until (eq *eof-value* sexp)
              collect sexp)))))

(defun create-file-snapshot (file &key (dest-directory "snapshots")
                                    (delimiter #\-))
  (when (and file
             (probe-file file))
    (let* ((snapshot-dir)
           (universal-time-now (get-universal-time))
           (timestamp
             (concatenate 'string
                          (format-timestring
                           nil (universal-to-timestamp
                                universal-time-now)
                           :format '((:year 4) #\-
                                     (:month 2) #\-
                                     (:day 2)))
                          "."
                          (write-to-string
                           (- universal-time-now
                              (timestamp-to-universal
                               (today)))))))
      (multiple-value-bind (directory full-file-name file-name file-type)
          (split-path file)
        (let ((dest-file
                (construct-destination-path
                 :source-path directory
                 :destination-directory dest-directory
                 :destination-file-name 
                 (construct-file-name "" "-"
                                      file-name "snapshot" timestamp)
                 :destination-file-extension file-type)))
          (rm-file dest-file)
          (and 
           (copy-file file dest-file)
           (probe-file dest-file)))))))

(defun construct-report-file-path (data-directory
                                &key (destination-directory "reports")
                                  destination-file-name destination-file-type
                                  (delimiter #\-))
  (when (directory-exists-p data-directory)
    (let ((reports-dir)
          (timestamp
            (format-timestring
             nil (now)
             :format '((:year 4) #\-
                       (:month 2) #\-
                       (:day 2)))))
      (let ((destination-file-path
              (construct-destination-path
               :source-path data-directory
               :destination-directory destination-directory
               :destination-file-name destination-file-name
               :destination-file-extension destination-file-type)))
        destination-file-path))))
