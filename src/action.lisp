(in-package :cl-user)
(defpackage action
  (:nicknames :act :ac)
  (:use :cl)
  (:import-from :uiop
                #:run-program)
  (:import-from :local-time
                #:now
                #:today
                #:format-timestring
                #:parse-timestring
                #:timestamp-difference)
  (:import-from :uuid
                #:make-uuid-from-string
                #:uuid=
                #:make-v4-uuid)
  (:import-from :alexandria
                #:flatten
                #:when-let)
  (:import-from :fad
                #:directory-exists-p)
  (:import-from :action/persistence
                #:write-sexp-to-file
                #:read-sexp-from-file
                #:create-file-snapshot
                #:construct-report-file-path)
  (:import-from :action/filesystem-interface
                #:rm-file)
  (:export
   #:system-version
   #:cli-list-actions
   #:cli-action-info
   #:add-action
   #:delete-action
   #:purge-action
   #:edit-action
   #:prepend-to-action
   #:append-to-action
   #:complete-action
   #:log-action
   #:annotate-action
   #:denotate-action
   #:edit-annotation
   #:start-action-time-log
   #:stop-action-time-log
   #:backup-file
   #:set-data-directory
   #:generate-action-list-report))
(in-package :action)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Miscellaneous utilities
(defun system-version (system-designator)
  (let ((system (asdf:find-system system-designator nil)))
    (when (and system (slot-boundp system 'asdf:version))
      (asdf:component-version system))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Data persistence

;; Action! will store its data in an XDG compliant data directory,
;; $HOME/.config/action/

;; Ensure all required data files exist, creating empty files if
;; necessary
(defun ensure-file-exists (file)
  (if (probe-file file)
      file
      (with-open-file (s file :direction :output)
        (write-string "" s))))

(defun ensure-files-exist (&rest files)
  (mapcar #'(lambda (file) (ensure-file-exists file))
          files))

(defparameter +actions-data-file+ ())
(defparameter +completed-actions-data-file+ ())
(defparameter +activity-log+ ())

(defparameter +project-data-directory+ ())

;; Ensure the directory exists, creating if if necessary
(defparameter +action-data-directory+ ())

(defparameter +iso-like-date-and-time-format+
  '((:year 4) #\- (:month 2) #\- (:day 2) #\Space
    (:hour 2) #\: (:min 2)))

(defun set-data-directory (&key parent-dir
                             (data-dir "action" data-dir-supplied-p))
  ""
  (if (and data-dir-supplied-p
           (stringp data-dir))
      (setf +action-data-directory+
            (ensure-directories-exist
             (if (uiop:absolute-pathname-p data-dir)
                 data-dir
                 (action/filesystem-interface:construct-directory 
                  *default-pathname-defaults* data-dir))))
      (setf +action-data-directory+
            (ensure-directories-exist
             (action/filesystem-interface:construct-directory 
              (uiop/configuration:xdg-data-home)
              data-dir))))
  (and
   (set-actions-data-file +action-data-directory+)
   (set-completed-actions-data-file +action-data-directory+)
   (set-activity-log-data-file +action-data-directory+)
   (ensure-files-exist +actions-data-file+
                       +completed-actions-data-file+
                       +activity-log+)))

;(merge-pathnames +action-data-directory+ "actions.data"))
(defun set-actions-data-file (data-dir)
  (setf +actions-data-file+
        (merge-pathnames data-dir "actions.data")))

;  (merge-pathnames +action-data-directory+ "completed.data"))
(defun set-completed-actions-data-file (data-dir)
  (setf +completed-actions-data-file+
        (merge-pathnames data-dir "completed.data")))

;  (merge-pathnames +action-data-directory+ "activity-log.data"))
(defun set-activity-log-data-file (data-dir)
  (setf +activity-log+
        (merge-pathnames data-dir "activity-log.data")))


(unless +action-data-directory+
  (set-data-directory))

(ensure-files-exist +actions-data-file+
                    +completed-actions-data-file+
                    +activity-log+)

;; Create *action-list* variable, holding a list of next actions
;; Load persisted actions from file upon first definition
(defvar *action-list* 'NIL)

(defun get-action-list ()
  (when (ensure-file-exists +actions-data-file+)
    (setf *action-list*
          (car
           (action/persistence:read-sexp-from-file +actions-data-file+)))))

(defun get-filtered-action-list (action-list)
  ""
  (when (and action-list (listp action-list))
    (remove-if
     #'(lambda (sublist)
         (when (and (getf sublist :wait)
                    (>
                     (timestamp-whole-day-difference
                      (today)
                      (parse-timestring
                       (getf sublist :wait)))
                     0))
           t))
     action-list)))

(defun get-sorted-action-list (action-list)
  ""
  (when (and action-list (listp action-list))
    (stable-sort action-list
                 #'>
                 :key #'(lambda (list) 
                          (+ (action::timestamp-whole-day-difference
                              (parse-timestring (getf list :created-on))
                              (today))
                             (if (integerp (getf list :priority))
                                 (- 100 (getf list :priority))
                                 0)
                             (if (parse-timestring 
                                  (getf list :due) :fail-on-error ())
                                 (action::timestamp-whole-day-difference 
                                  (getf list :due) 
                                  (today))
                                 0)
                             (if (parse-timestring
                                  (getf list :wait) 
                                  :fail-on-error ())
                                 (* -1 
                                    (action::timestamp-whole-day-difference
                                     (today)
                                     (parse-timestring
                                      (getf list :wait))))
                                 0))))))

;; Upon every setting of the action list, persist the list to file
(defun set-action-list (new-value)
  (action/persistence:write-sexp-to-file +actions-data-file+
                                         (setf *action-list* new-value)))

(defun add-action-to-action-list (action)
  (and
   (append-to-activity-log action :CREATE-NEW-ACTION)
   (action/persistence:write-sexp-to-file
    +actions-data-file+
    (set-action-list
     (cons action (get-action-list))))))

;; Create *completed-actions-list* variable, holding a list of all
;; completed actions. DO NOT load the file, as it is of unbounded
;; length
(defvar *completed-actions-list* ())

(defun get-completed-actions-list ()
  (when (null *completed-actions-list*)
    (lazy-load-completed-actions))
  *completed-actions-list*)

(defun lazy-load-completed-actions ()
  (setf *completed-actions-list*
        (action/persistence:read-sexp-from-file
         +completed-actions-data-file+)))

(defun add-action-to-completed-actions-list (action)
  (progn
    (lazy-load-completed-actions)
    (let ((completed-action (append action
                                    (list :status "completed"
                                          :completed-on
                                          (format-timestring 'NIL (now))))))
      (and
       (append-to-activity-log completed-action :COMPLETE-ACTION
                               :old-action action)
       (action/persistence:write-sexp-to-file
        +completed-actions-data-file+ completed-action
        :exists-action :append)
       (push completed-action *completed-actions-list*)
       t))))

;; Create *activity-log* variable, holding a list of all
;; completed actions. DO NOT load the file, as it is of unbounded
;; length
(defvar *activity-log* ())

(defun append-to-activity-log (action activity-type &key old-action)
  (when
      (and (listp action)
           (keywordp activity-type)
           (if old-action
               (listp old-action)
               t))
    (when-let ((action-id (getf action :id)))
      (let ((activity-to-log
              (cond (old-action (list :time (format-timestring 'NIL (now))
                                      :action-id action-id
                                      :activity activity-type
                                      :from old-action
                                      :to action))
                    (t (list :time (format-timestring 'NIL (now))
                             :action-id action-id
                             :activity activity-type
                             :action action)))))
        (action/persistence:write-sexp-to-file
         +activity-log+ activity-to-log
         :exists-action :append)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Define main action verbs
(defun add-action (description &key (priority "") (time-estimated ""))
  (let ((timestamp (format-timestring 'NIL (now)))
        (uuid (intern (format nil "~s" (make-v4-uuid)))))
    (and
     (add-action-to-action-list
      (list :id uuid :priority priority
            :time-estimated time-estimated :description description 
            :created-on timestamp))
     (shorten-id uuid 2))))

(defun action-list (&key (action-list *action-list*)
                       (completed-actions-list *completed-actions-list*)
                       list-completed)
  (if list-completed
      (get-completed-actions-list)
      (get-action-list)))

(defun shorten-id (id &optional (digits 2))
  (subseq
   (symbol-name id)
   0 digits))

(defun get-id-from-fragment (short-id
                             &key (action-list (get-action-list))
                               (short-id-length 2))
  (when (and short-id
             (or (stringp short-id)
                 (setf short-id (format nil "~a" short-id))))
    (setf short-id (string-upcase short-id))
    (second
     (first
      (remove-if-not
       #'(lambda (i) (string= (car i) short-id))
       (mapcar #'(lambda (action) (list (subseq
                                       (format nil "~A"
                                               (getf action :id))
                                       0 short-id-length)
                                      (getf action :id)))
               action-list))))))

(defun timestamp-whole-day-difference (time-a time-b)
  "Returns the number of whole days elapsed between time-a and time-b"
  (when (and time-a time-b)
    (let* ((seconds-in-day (* 60 60 24)))
      (ceiling
       (/ 
        (timestamp-difference 
         (if (stringp time-b)
             (parse-timestring time-b)
             time-b)
         (if (stringp time-a)
             (parse-timestring time-a)
             time-a))
        seconds-in-day)))))

(defun calculate-action-information (action-sublist)
  ""
  (when (listp action-sublist)
    (let ((today (today)))
      (append action-sublist
              (list
               :short-id (if (stringp (getf action-sublist :id))
                             (shorten-id (intern (getf action-sublist :id)) 2)
                             (shorten-id (getf action-sublist :id) 2))
               :numeric-priority (if (stringp (getf action-sublist :priority))
                                     0
                                     (getf action-sublist :priority))
               :action-age (timestamp-whole-day-difference
                            (parse-timestring (getf action-sublist :created-on))
                            today)
               :last-modified-on (if (getf action-sublist :modified-on)
                                     (getf action-sublist :modified-on)
                                     "")
               :last-modification-age (if (getf action-sublist :modified-on)
                                          (timestamp-whole-day-difference
                                           (parse-timestring
                                            (getf action-sublist :modified-on))
                                           today)
                                          "")
               :wait-until (if (parse-timestring
                               (getf action-sublist :wait)
                               :fail-on-error NIL)
                               (parse-timestring
                                (getf action-sublist :wait))
                               "")
               :wait-days (if (parse-timestring
                               (getf action-sublist :wait)
                               :fail-on-error NIL)
                              (timestamp-whole-day-difference
                               today
                               (parse-timestring
                                (getf action-sublist :wait)))
                              "")
               :due-on (if (parse-timestring
                            (getf action-sublist :due)
                            :fail-on-error NIL)
                           (parse-timestring
                            (getf action-sublist :due))
                           "")
               :due-on-year (if (parse-timestring
                                 (getf action-sublist :due)
                                 :fail-on-error NIL)
                                (format-timestring NIL
                                                   (parse-timestring
                                                    (getf action-sublist :due))
                                                   :format '((:year 4)))
                                "")
               :due-on-month (if (parse-timestring
                                  (getf action-sublist :due)
                                  :fail-on-error NIL)
                                 (format-timestring NIL
                                  (parse-timestring
                                   (getf action-sublist :due))
                                  :format '((:month 2)))
                                 "")
               :due-on-date (if (parse-timestring
                                  (getf action-sublist :due)
                                  :fail-on-error NIL)
                                 (format-timestring NIL
                                  (parse-timestring
                                   (getf action-sublist :due))
                                  :format '((:day 2)))
                                 "")
               :due-days (if (parse-timestring
                              (getf action-sublist :due)
                              :fail-on-error NIL)
                             (timestamp-whole-day-difference
                              today
                              (parse-timestring
                               (getf action-sublist :due)))
                             ""))))))

(defun columns-to-keywords (column-list)
  (mapcar #'(lambda (column)
              (cond ((keywordp column) column)
                    ((symbolp column) (intern (symbol-name column) :keyword))
                    ((stringp column) (intern (string-upcase column) :keyword))))
          column-list))

(defun get-action-columns (action-sublist &rest columns)
  ""
  (let ((keyword-columns
          (columns-to-keywords columns))
        (calculated-list
          (calculate-action-information action-sublist)))
    (mapcar #'(lambda (column)
                (getf calculated-list column))
            keyword-columns)))

(defun get-action-columns-and-headings (action-sublist columns-and-headings-list)
  ""
  (when (listp columns-and-headings-list)
    (let* ((unzipped-list (loop for i
                                  in columns-and-headings-list
                                collect (car i) into label
                                collect (cdr i) into column
                                finally (return
                                          (list label column))))
           (keyword-columns
             (columns-to-keywords (cadr unzipped-list)))
           (calculated-list
             (calculate-action-information action-sublist))
           (returned-columns
             (mapcar #'(lambda (column)
                         (getf calculated-list column))
                     keyword-columns)))
      (flatten
       (mapcar #'list
               (car unzipped-list)
               returned-columns)))))

(defun cli-list-actions (&key list-completed)
  (let ((today (today)))
    (flet ((format-header ()
             (format t "ID Pri Due Description~%")
             (format t "-- --- --- -----------~%"))
           (format-action-list (&key list-completed)
             (if list-completed
                 (mapcar #'(lambda (action)
                             (get-action-columns action
                                                 'id 'priority 'due 'description))
                         (get-completed-actions-list))
                 (mapcar #'(lambda (action)
                             (get-action-columns
                              (calculate-action-information action)
                              'short-id 'priority 'due-days 'description))
                         (get-sorted-action-list
                          (get-filtered-action-list
                           (get-action-list)))))))
      (progn
        (format-header)
        (format t "~:{~&~2A ~3a ~3a ~A~}"
                (format-action-list :list-completed list-completed))
        (terpri)))))

(defun cli-action-info (id)
  ""
  (when id
    (let ((canonical-id (string-upcase id)))
      (when-let ((matching-action (get-action-by-id canonical-id)))
        (format t "~%Task ~a information~%" canonical-id)
        (format t "===================~%~%" canonical-id)
        (format t "~{~20a ~a~%~}~%"
                (get-action-columns-and-headings
                 matching-action
                 '(("Short Id" . short-id) ("UUID" . id)
                   ("Description"  . description)
                   ("Created on" . created-on)
                   ("Action age" . action-age)
                   ("Wait until" . wait-until) ("Days to wait" . wait-days)
                   ("Due" . due-on) ("Due in (days)" . due-days)
                   ("Last modified on" . last-modified-on)
                   ("Modified (days ago)" . last-modification-age))))
        (when-let ((annotations (getf matching-action :annotations)))
          (format t "Annotations~%")
          (format t "===========~%~%")
          (format t "ID  Date and time     Note~%")
          (format t "--  ----------------  -----~%")
          (format t "~{~{~2d  ~16a  ~a ~%~}~}~%"
                  (mapcar 
                   #'(lambda (annotation)
                       (list (getf annotation :annotation-id)
                             (format-timestring 'NIL
                                                (parse-timestring
                                                 (getf annotation :created-on))
                                                :format +iso-like-date-and-time-format+)
                             (getf annotation :note)))
                   annotations)))
        (when-let ((time-log (getf matching-action :time-log)))
          (format t "Time logged~%")
          (format t "===========~%~%")
          (format t "Start time        End time          Duration (hh:mm)~%")
          (format t "----------------  ----------------  ----------------~%")
          (format t "~{~{~16a  ~16a  ~2d:~2,'0d~%~}~}~%"
                  (mapcar 
                   #'(lambda (time-log)
                       (list 
                        (format-timestring 'NIL 
                                                (parse-timestring (getf time-log :start))
                                                :format +iso-like-date-and-time-format+)
                        (if (getf time-log :stop) 
                            (format-timestring 'NIL 
                                               (parse-timestring (getf time-log :stop))
                                               :format +iso-like-date-and-time-format+)
                            "")
                        (getf (format-time-duration
                               (getf time-log :duration)) :hours)
                        (getf (format-time-duration
                               (getf time-log :duration)) :minutes)))
                   time-log))
          (format t "=== Total time logged (hh:mm): ~2d:~2,'0d ===~%"
                  (getf (format-time-duration
                         (get-action-time-logged :action-list matching-action)) :hours)
                  (getf (format-time-duration
                         (get-action-time-logged :action-list matching-action)) :minutes)))))))

(defun get-action-by-id (id &key (action-list (get-action-list)))
  (when id
    (when-let ((expanded-id (get-id-from-fragment id)))
      (find-if
       #'(lambda (action) (uuid=
                           (make-uuid-from-string
                            (symbol-name expanded-id))
                           (make-uuid-from-string
                            (symbol-name (getf action :id)))))
       action-list))))

(defun remove-action (id &key (action-list (get-action-list)))
  (when id
    (when-let ((matching-action (get-action-by-id id)))
      (set-action-list
       (remove-if
        #'(lambda (action) (equal action matching-action))
        action-list)))))

(defun delete-action (id &key (reason "") (purge ()))
  (when id
    (let ((canonical-id (string-upcase id)))
      (when-let ((matching-action (get-action-by-id canonical-id)))
        (let ((deleted-action
                (append matching-action
                        (list :status "deleted"
                              :reason reason
                              :deleted-on
                              (format-timestring 'NIL (now))))))
          (if purge
              (remove-action canonical-id)
              (and
               (append-to-activity-log deleted-action :DELETE-ACTION
                                       :old-action matching-action)
               (remove-action canonical-id))))))))

(defun purge-action (id)
  (when id
    (when-let ((snapshot-file
                (create-file-snapshot +ACTIVITY-LOG+
                                      :dest-directory "tmp")))
      (progn 
        (rm-file +activity-log+)
        (with-open-file (in snapshot-file)
          (with-open-file (out +activity-log+
                               :direction :output
                               :if-exists :supersede)
            (with-standard-io-syntax
              (let ((*print-circle* t))
                (loop with eof = (gensym)
                      for object = (read in nil eof)
                      until (eq object eof)
                      do (unless (equal (getf object :action-id)
                                        (get-id-from-fragment id))
                           (print object out)))))))
        (and 
         (delete-action id :purge t)
         (rm-file snapshot-file))
        id))))

(defun edit-action (id &key description merge-with-description
                      priority time-estimated due wait)
  (when id
    (let ((canonical-id (string-upcase id)))
      (when-let ((matching-action (get-action-by-id canonical-id)))
        (let ((updated-action (copy-list matching-action)))
          (when (and description (stringp description))
            (cond ((eq merge-with-description :prepend)
                   (setf (getf updated-action :description)
                         (concatenate 'string
                                      description " "
                                      (getf updated-action :description))))
                  ((eq merge-with-description :append)
                   (setf (getf updated-action :description)
                         (concatenate 'string
                                      (getf updated-action :description)
                                      " " description)))
                  (t (setf (getf updated-action :description) description))))
          (when priority
            (setf (getf updated-action :priority) priority))
          (when time-estimated
            (setf (getf updated-action :time-estimated) time-estimated))
          (when (and due (parse-timestring due :fail-on-error NIL))
            (if (getf updated-action :due)
                (setf (getf updated-action :due) due)
                (setf updated-action
                      (append updated-action (list :due due)))))
          (when (and wait (parse-timestring wait :fail-on-error NIL))
            (if (getf updated-action :wait)
                (setf (getf updated-action :wait) wait)
                (setf updated-action
                      (append updated-action (list :wait wait)))))
          (if (getf updated-action :modified-on)
              (setf (getf updated-action :modified-on)
                    (format-timestring 'NIL (now)))
              (setf updated-action
                    (append updated-action
                            (list :status "modified"
                                  :modified-on
                                  (format-timestring 'NIL (now))))))
          (and
           (append-to-activity-log updated-action :MODIFY-ACTION
                                   :old-action matching-action)
           (set-action-list
            (mapcar #'(lambda (action)
                        (if
                         (equal (get-action-by-id canonical-id) action)
                         updated-action
                         action))
                    (get-action-list)))
           canonical-id))))))

(defun prepend-to-action (id text)
  (when (and id
             (plusp (length text)))
    (let ((canonical-id (string-upcase id)))
      (edit-action canonical-id :description text
                                :merge-with-description :prepend))))

(defun append-to-action (id text)
  (when (and id
             (plusp (length text)))
    (let ((canonical-id (string-upcase id)))
      (edit-action canonical-id :description text
                                :merge-with-description :append))))

(defun complete-action (id)
  (when id
    (let ((canonical-id (string-upcase id)))
      (when-let ((matching-action (get-action-by-id canonical-id)))
        (and
         (add-action-to-completed-actions-list matching-action)
         (remove-action canonical-id))
        canonical-id))))

(defun log-action (description &key (log-date NIL) (priority "") (time-estimated 0))
  "Log action which is already completed, without creating it first,
then completing it. The purpose is to keep a log of actions undertaken
and completed, even if some of them weren't managed from within Action!"
  (progn
    (lazy-load-completed-actions)
    (let* ((timestamp
             (format-timestring 'NIL
                                (if (and log-date
                                         (parse-timestring log-date :fail-on-error NIL))
                                    (parse-timestring log-date) 
                                    (now))))
           (uuid
             (intern (format nil "~s" (make-v4-uuid))))
           (completed-action
             (list :id uuid :priority priority
                   :time-estimated time-estimated :description description 
                   :created-on timestamp
                   :status "logged completed"
                   :completed-on timestamp)))
      (and
       (append-to-activity-log completed-action :LOG-ACTION)
       (action/persistence:write-sexp-to-file
        +completed-actions-data-file+ completed-action
        :exists-action :append)
       (push completed-action *completed-actions-list*)
       uuid ))))

(defun get-max-annotation-id (&key action-id action-list)
  (flet ((max-annotation-id (action-list)
           (apply #'max
                  (mapcar #'(lambda (annotations)
                              (getf annotations :annotation-id))
                          (getf action-list :annotations)))))
    (cond (action-id
           (let ((canonical-id (string-upcase action-id)))
             (when-let ((matching-action (get-action-by-id canonical-id)))
               (max-annotation-id matching-action))))
          ((and action-list (listp action-list))
           (max-annotation-id action-list)))))

(defun annotate-action (id note)
  ""
  (when (and (stringp id) (not (zerop (length id)))
             (stringp note) (not (zerop (length note))))
    (let ((canonical-id (string-upcase id)))
      (when-let ((matching-action (get-action-by-id canonical-id)))
        (let* ((updated-action (copy-list matching-action))
               (annotations (getf updated-action :annotations)))
          (if annotations
              (setf (getf updated-action :annotations)
                    (append annotations
                            (list
                             (list :annotation-id (1+
                                                   (get-max-annotation-id
                                                    :action-list updated-action))
                                   :note note
                                   :created-on (format-timestring 'NIL
                                                                  (now))))))
              (setf updated-action
                    (append updated-action
                            (list :annotations
                                  (list
                                   (list :annotation-id 1
                                         :note note
                                         :created-on (format-timestring 'NIL
                                                                        (now))))))))
          (and
           (append-to-activity-log updated-action :ANNOTATE-ACTION
                                   :old-action matching-action)
           (set-action-list
            (mapcar #'(lambda (action)
                        (if
                         (equal (get-action-by-id canonical-id) action)
                         updated-action
                         action))
                    (get-action-list)))
           canonical-id))))))

(defun get-incomplete-time-log (&key action-id action-list)
  (flet ((incomplete-time-log (action-list)
           (car
            (remove-if #'null
                       (mapcar
                        #'(lambda (time-log) (when
                                                 (and (null (getf time-log :stop))
                                                      (zerop (getf time-log :duration)))
                                               time-log))
                        (getf action-list :time-log))))))
    (cond (action-id
           (let ((canonical-id (string-upcase action-id)))
             (when-let ((matching-action (get-action-by-id canonical-id)))
               (incomplete-time-log matching-action))))
          ((and action-list (listp action-list))
           (incomplete-time-log action-list)))))

(defun get-action-time-logged (&key action-id action-list)
  (flet ((time-logged (action-list)
           (apply #'+
                  (mapcar 
                   #'(lambda (time-log)
                       (getf time-log :duration))
                   (getf action-list :time-log)))))
    (cond (action-id
           (let ((canonical-id (string-upcase action-id)))
             (when-let ((matching-action (get-action-by-id canonical-id)))
               (time-logged matching-action))))
          ((and action-list (listp action-list))
           (time-logged action-list)))))

(defun format-time-duration (duration)
  (list
   :hours (floor (/ duration (* 60 60)))
   :minutes (floor (/ duration 60))
   :seconds (rem duration 60)))

(defun log-action-time (id entry-type)
  (when (and (stringp id) (not (zerop (length id)))
             (keywordp entry-type))
    (let ((canonical-id (string-upcase id)))
      (when-let ((matching-action (get-action-by-id canonical-id)))
        (let* ((updated-action (copy-list matching-action))
               (time-log (getf updated-action :time-log)))
          (if time-log
              (cond ((eq entry-type :start)
                     (unless (get-incomplete-time-log :action-list updated-action)
                       (setf (getf updated-action :time-log)
                             (append time-log
                                     (list
                                      (list :start (format-timestring 'NIL
                                                                      (now))
                                            :stop ()
                                            :duration 0))))))
                    ((eq entry-type :stop)
                     (when (get-incomplete-time-log :action-list updated-action)
                       (let* ((incomplete-time-log
                                (get-incomplete-time-log :action-list updated-action))
                              (timestamp (now))
                              (duration
                                (floor (local-time:timestamp-difference
                                        timestamp
                                        (parse-timestring
                                         (getf incomplete-time-log :start))))))
                         (setf
                          (getf updated-action :time-log)
                          (mapcar #'(lambda (time-log)
                                      (if
                                       (equal time-log incomplete-time-log)
                                       (list
                                        :start (getf time-log :start)
                                        :stop (format-timestring 'NIL timestamp)
                                        :duration duration)
                                       time-log))
                                  (getf updated-action :time-log)))))))
              (when (eq entry-type :start)
                (setf updated-action
                      (append updated-action
                              (list :time-log
                                    (list
                                     (list :start (format-timestring 'NIL
                                                                     (now))
                                           :stop ()
                                           :duration 0)))))))
          (and
           (append-to-activity-log updated-action :LOG-ACTION-TIME
                                   :old-action matching-action)
           (set-action-list
            (mapcar #'(lambda (action)
                        (if
                         (equal (get-action-by-id canonical-id) action)
                         updated-action
                         action))
                    (get-action-list)))
           canonical-id))))))

(defun start-action-time-log (id)
  (log-action-time id :start))

(defun stop-action-time-log (id)
  (log-action-time id :stop))

(defun backup-file (data-file)
  ""
  (when data-file
    (cond ((eq data-file :actions)
           (create-file-snapshot +ACTIONS-DATA-FILE+
                                 :dest-directory "backup"))
          ((or
            (eq data-file :completed-actions)
            (eq data-file :completed)
            (eq data-file :done))
           (create-file-snapshot +COMPLETED-ACTIONS-DATA-FILE+
                                 :dest-directory "backup"))
          ((or
            (eq data-file :activity-log)
            (eq data-file :activity)
            (eq data-file :log))
           (create-file-snapshot +ACTIVITY-LOG+
                                 :dest-directory "backup"))
          ((eq data-file :all)
           (and
            (create-file-snapshot +ACTIONS-DATA-FILE+
                                  :dest-directory "backup")
            (create-file-snapshot +COMPLETED-ACTIONS-DATA-FILE+
                                  :dest-directory "backup")
            (create-file-snapshot +ACTIVITY-LOG+
                                  :dest-directory "backup"))))))

;; I was considering whether to use YAML for data (activity)
;; serialization to disk, specifically whether to use the MAP
;; or DOCUMENT type for each entry
;;
;; Either way, if the information originates in a plist, then
;; the following code (using cl-yaml) works bi-directionally:
;;
;; (yaml:emit-to-string (alexandria:plist-hash-table
;;   (alexandria:hash-table-plist (yaml:parse "{ a: 1, b: 2 }"))))
;;
;; In conclusion, I now believe that this is a premature optimisation
;; and an unnecessary complication and burden; it's much easier
;; by starting to serialize the native Lisp list to text.

(defun generate-action-list-report-source-file ()
  (let ((actions-file
          (construct-report-file-path 
           +action-data-directory+
           :destination-file-name "action-entries"
           :destination-file-type "tex")))
    (with-open-file (file
                     actions-file
                     :direction :output
                     :if-exists :supersede
                     :if-does-not-exist :create)
      (with-standard-io-syntax
        (format file "~{~a~%~}"
                (list
                 (format-timestring nil 
                                    (today)
                                    :format  '("\\def \\reportyear {" :year "}" #\Newline
                                               "\\def \\reportmonthnum {" :month "}" #\Newline
                                               "\\def \\reportlongmonthname {" :long-month "}" #\Newline
                                               "\\def \\reportshortmonthname {" :short-month "}" #\Newline
                                               "\\def \\reportday {" :day "}" #\Newline
                                               "\\def \\reportweekdaynum {" :weekday "}" #\Newline
                                               "\\def \\reportordinalday {" :ordinal-day "}" #\Newline
                                               "\\def \\reportlongweekdayname {" :long-weekday "}" #\Newline
                                               "\\def \\reportshortweekdayname {" :short-weekday "}" #\Newline
                                               "\\def \\reportminimalweekdayname {" :minimal-weekday "}" #\Newline
                                               "\\def \\reportweeknum {" :iso-week-number "}" #\Newline))
                 "\\def \\actionprint {"
                 (format nil "~{~{\\trd{~a}{0}{~d}{~a}{~a}{~a}{~a}{~a}{}~%~}~}"
                         (cadr (format-action-list-for-report)))))
        (dotimes (i (- 30 (car (format-action-list-for-report))))
          (format file "\\trd{}{0}{}{}{}{}{}{}{}~%"))
        (format file "~{~a~%~}"
                '("\\arrayrulecolor{black}"
                  "}"))))
    (probe-file actions-file)))

(defun format-action-list-for-report ()
  ""
  (flet ((count-action-desc-lines (action-list)
           (apply #'+ 
                  (mapcar 
                   #'(lambda (i) (ceiling (/ (length (nth 3 i)) 70)))
                   action-list))))
    (let ((formatted-action-list
            (mapcar #'(lambda (action)
                        (get-action-columns
                         (calculate-action-information action)
                         'short-id 'priority 'time-estimated
                         'description 'due-on-year
                         'due-on-month 'due-on-date))
                    (get-sorted-action-list
                     (get-filtered-action-list
                      (get-action-list))))))
      (list
       (count-action-desc-lines formatted-action-list)
       formatted-action-list))))

(defun run-cmd (command-list &key (output NIL))
  (when (and (listp command-list)
             (< 0 (length command-list)))
    (multiple-value-bind (output-returned error-output return-value)
        (run-program command-list
                     :ignore-error-status t
                     :output output)
      (list output-returned error-output return-value))))

(defun generate-action-list-report ()
  (let ((latex-bin (run-cmd '("which" "lualatex")))
        (report-file
          (construct-report-file-path 
           +action-data-directory+
           :destination-file-name "action-list"
           :destination-file-type "pdf")))
    (when (zerop (third latex-bin))
      (when-let ((tex-file-name 
                  (namestring
                   (third
                    ;; generate-helper-files
                    (generate-action-list-report-helper-files)))))

        ;; generate-source-file
        (generate-action-list-report-source-file)

        ;; run lualatex
        (when (zerop
               (third
                (run-cmd (list "lualatex"
                               (concatenate 'string 
                                            "-output-directory="
                                            (directory-namestring report-file))
                               tex-file-name))))
          ;; check if pdf file has been generated, returning its full path
          (and (probe-file report-file)
               report-file))))))

(defun generate-action-list-report-helper-files ()
  (let ((actions-file
          (construct-report-file-path 
           +action-data-directory+
           :destination-file-name "action-entries"
           :destination-file-type "tex"))
        (tex-file
          (construct-report-file-path 
           +action-data-directory+
           :destination-file-name "action-list"
           :destination-file-type "tex"))
        (lua-file
          (construct-report-file-path 
           +action-data-directory+
           :destination-file-name "action-list"
           :destination-file-type "lua")))
    (list
     (if (not (probe-file tex-file))
         (progn
           (with-open-file (file
                            tex-file
                            :direction :output
                            :if-exists :supersede
                            :if-does-not-exist :create)
             (with-standard-io-syntax
               (format file "~{~a~%~}"
                       (list
                        "% !TEX TS-program = lualatex"
                        "% !TEX encoding = UTF-8 Unicode"
                        "\\documentclass[a4paper,english]{article}"
                        "% LaTeX To Do List"
                        "% Copyright © 2013–2017, Chris Warrick."
                        "% See /LICENSE (in the distribution) for licensing information."
                        (format nil "\\def \\actionentriespath  {~a}" actions-file)
                        "\\def \\monofont     {AnonymousPro}"
                        "\\input{\\actionentriespath}"
                        "\\usepackage{fontspec}"
                        "\\usepackage{polyglossia}"
                        "\\setmainfont[Ligatures=TeX]{NormativeLt-Regular}" ; Use Advocate's condensed typeface
                        "\\def \\mainfontsize {29.955em}"
                        "\\def \\titleskip {5.91pt}"
                        "\\def \\actionsub  {\\hskip1em|\\kern-1.5pt--\\space}"
                        "\\usepackage{luacode}"
                        (format nil "\\luadirect{require(\"~a\")}" lua-file)
                        "\\setmonofont[]{\\monofont}"
                        "\\setcounter{secnumdepth}{5}"
                        "\\setcounter{tocdepth}{5}"
                        "\\usepackage{color}"
                        "\\usepackage[usenames,dvipsnames,svgnames,table]{xcolor}"
                        "\\usepackage{setspace}"
                        "\\linespread{1.50}" ; Reduce linespacing from double
                        "\\usepackage[parfill]{parskip}"
                        "\\setlength{\\parskip}{\\smallskipamount}"
                        "\\setlength{\\parindent}{0pt}"
                        "\\usepackage[unicode, colorlinks, breaklinks, pdftitle={Action! Daily planner},pdfauthor={Action!}]{hyperref}"
                        "\\usepackage{upquote}"
                        "\\date{}"
                        "\\makeatother"
                        "\\usepackage[twoside=true,total={16.15cm,24.23cm},top=1.615cm,inner=2.6cm,bottom=3.231cm,outer=2.245cm]{geometry}"
                        "\\nonfrenchspacing"
                        "\\usepackage{titlesec}"
                        "\\setlength{\\fboxsep}{1pt}"
                        "\\newcommand{\\sps}[0]{\\hskip2pt}"
                        "\\definecolor{ngray}{gray}{0.30}"
                        "\\definecolor{tabgray}{gray}{0.65}"
                        "\\definecolor{backgray}{gray}{0.55}"
                        "\\definecolor{dategray}{gray}{0.85}"
                        "\\newcommand{\\fcb}[1]{\\fcolorbox{backgray}{black}{#1}}"
                        "\\newcommand{\\trd}[8]{\\luadirect{actionrow(\\luastringN{#1}, \\luastringN{#2}, \\luastringN{#3}, \\luastringN{#4}, \\luastringN{#5}, \\luastringN{#6}, \\luastringN{#7}, \\luastringN{#8})}}"
                        "\\newcommand{\\tr}[1]{\\luadirect{actionrow(#1, \"\", \"\", \"\", \"\", \"\", \"\", \"\")}}"
                        "\\newcommand{\\nr}[1]{\\arrayrulecolor{tabgray}\\multicolumn{10}{l}{#1}\\\\\\hline}"
                        "\\newcommand{\\p}[1]{\\texttt{#1}}"
                        "\\newfontfamily\\headingdatefont[Ligatures=TeX]{NormativeLt-Regular}"
                        "\\newfontfamily\\headingthinfont[Ligatures=TeX]{NormativeLt-Light}"
                        "\\newfontfamily\\subheadfont[Ligatures=TeX]{Advocate C43}"
                        "\\makeatletter"
                        "\\begin{document}"
                        "{\\headingdatefont\\fontsize{36pt}{30pt}\\selectfont\\reportday\\par}"
                        "{\\headingthinfont\\fontsize{18pt}{14pt}\\selectfont\\reportlongweekdayname\\\\\\reportlongmonthname\\hskip0.5ex\\reportyear\\par}"
                        "\\vspace{0.75em}"
                        "{\\subheadfont\\fontsize{16pt}{12pt}\\selectfont ACTION LIST\\par}"
                        "\\vspace{0.15em}"
                        "\\thispagestyle{empty}"
                        "\\begin{tabular}{@{}p{1.5em}@{} @{}c@{} @{}c@{} @{}p{3em}@{} p{\\mainfontsize} @{}c@{} @{}p{1em}@{} @{}p{1em}@{}  @{}p{1em}@{} @{}p{1em}@{}}"
                        "\\# & \\centering C & \\centering Priority & \\centering Time & Task &\\multicolumn{5}{l}{Due Date}"
                        "\\\\\\hline \\arrayrulecolor{tabgray}"
                        "\\actionprint"
                        "\\end{tabular}"
                        "\\end{document}"))))
           tex-file)
         t)

     (if (not (probe-file lua-file))
         (progn
           (with-open-file (file
                            lua-file
                            :direction :output
                            :if-exists :supersede
                            :if-does-not-exist :create)
             (with-standard-io-syntax
               (format file "~{~a~%~}"
                       (list
                        "-- LaTeX To Do List (Lua companion)"
                        "-- Copyright © 2013–2017, Chris Warrick."
                        "-- See /LICENSE (in the distribution) for licensing information."
                        "PLACEHOLDER = \"\\\\textcolor{backgray}{%s}\""
                        "function actionrow(number, completed, priority, time, task, year, month, day)"
                        "    if completed == \"1\" then"
                        "        completed_entry = \"\\\\fcb{C}\""
                        "    else"
                        "        completed_entry = \"\\\\fbox{C}\""
                        "    end"
                        "    if priority == \"1\" then"
                        "        priority_entry = \"\\\\sps\\\\fcb{1}\\\\sps\\\\fbox{2}\\\\sps\\\\fbox{3}\\\\sps\""
                        "    elseif priority == \"2\" then"
                        "        priority_entry = \"\\\\sps\\\\fbox{1}\\\\sps\\\\fcb{2}\\\\sps\\\\fbox{3}\\\\sps\""
                        "    elseif priority == \"3\" then"
                        "        priority_entry = \"\\\\sps\\\\fbox{1}\\\\sps\\\\fbox{2}\\\\sps\\\\fcb{3}\\\\sps\""
                        "    else"
                        "        priority_entry = \"\\\\sps\\\\fbox{1}\\\\sps\\\\fbox{2}\\\\sps\\\\fbox{3}\\\\sps\""
                        "    end"
                        "    if time == \"0\" then"
                        "        time = string.format(PLACEHOLDER, \"  \")"
                        "    end"
                        "    if year == \"\" then"
                        "        due_year = string.format(PLACEHOLDER, \"YYYY\")"
                        "    else"
                        "        due_year = year"
                        "    end"
                        "    if month == \"\" then"
                        "        month1 = string.format(PLACEHOLDER, \"M\")"
                        "        month2 = month1"
                        "    else"
                        "        if string.len(month) == 1 then"
                        "            month = \"0\" .. month"
                        "        end"
                        "        month1 = string.sub(month, 1, 1)"
                        "        month2 = string.sub(month, 2, 2)"
                        "    end"
                        "    if day == \"\" then"
                        "        day1 = string.format(PLACEHOLDER, \"D\")"
                        "        day2 = day1"
                        "    else"
                        "        if string.len(day) == 1 then"
                        "            day = \"0\" .. day"
                        "        end"
                        "        day1 = string.sub(day, 1, 1)"
                        "        day2 = string.sub(day, 2, 2)"
                        "    end"
                        "    tex.print(string.format([[%s&\\centering\\textcolor{backgray}{\\sps%s\\sps}&\\textcolor{backgray}{%s}&\\centering{}%s&%s&\\phantom{|}%s\\phantom{|}&\\centering{}%s&\\centering{}%s&\\centering{}%s&%s\\\\\\hline]],"
                        "        number, completed_entry, priority_entry, time, task, due_year, month1, month2, day1, day2))"
                        "end"))))
           lua-file)
         t)
     tex-file)))
