(in-package :cl-user)
(defpackage action
  (:nicknames :act :ac)
  (:use :cl)
  (:import-from :local-time
                #:now
                #:format-timestring)
  (:import-from :uuid
                #:make-uuid-from-string
                #:uuid=
                #:make-v4-uuid)
  (:import-from :alexandria
                #:when-let)
  (:import-from :fad
                #:directory-exists-p)
  (:import-from :action/persistence
                #:write-sexp-to-file
                #:read-sexp-from-file
                #:create-file-snapshot)
  (:import-from :action/filesystem-interface
                #:rm-file)
  (:export
   #:system-version
   #:add-action
   #:cli-list-actions
   #:delete-action
   #:purge-action
   #:edit-action
   #:prepend-to-action
   #:append-to-action
   #:complete-action
   #:log-action
   #:backup-file
   #:set-data-directory))
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
                      (local-time:today)
                      (local-time:parse-timestring
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
                              (local-time:parse-timestring (getf list :created-on))
                              (local-time:today))
                             (if (integerp (getf list :priority))
                                 (- 100 (getf list :priority))
                                 0)
                             (if (local-time:parse-timestring 
                                  (getf list :due) :fail-on-error ())
                                 (action::timestamp-whole-day-difference 
                                  (getf list :due) 
                                  (local-time:today))
                                 0)
                             (if (local-time:parse-timestring
                                  (getf list :wait) 
                                  :fail-on-error ())
                                 (* -1 
                                    (action::timestamp-whole-day-difference
                                     (local-time:today)
                                     (local-time:parse-timestring
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
(defun add-action (description &key (priority "") (time-estimated 0))
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
      (/ 
       (local-time:timestamp-difference 
        (if (stringp time-b)
            (local-time:parse-timestring time-b)
            time-b)
        (if (stringp time-a)
            (local-time:parse-timestring time-a)
            time-a))
        seconds-in-day))))

(defun cli-list-actions (&key list-completed)
  (let ((today (local-time:today)))
    (flet ((format-header ()
             (format t "ID Pri Wait Due Description~%")
             (format t "-- --- ---- --- -----------~%"))
           (format-action-list (&key list-completed)
             (mapcar
              #'(lambda (action) (list
                                  (if (stringp (getf action :id))
                                      (shorten-id (intern (getf action :id)) 2)
                                      (shorten-id (getf action :id) 2))
                                  (if (stringp (getf action :priority))
                                      0 (getf action :priority))
                                  (if (local-time:parse-timestring
                                       (getf action :wait)
                                       :fail-on-error NIL)
                                      (timestamp-whole-day-difference
                                       today
                                       (local-time:parse-timestring
                                        (getf action :wait)))
                                      "")
                                  (if (local-time:parse-timestring
                                       (getf action :due)
                                       :fail-on-error NIL)
                                      (timestamp-whole-day-difference
                                       today
                                       (local-time:parse-timestring
                                        (getf action :due)))
                                      "")
                                  (getf action :description)))
              (if list-completed
                  (get-completed-actions-list)
                  (get-sorted-action-list
                   (get-filtered-action-list
                    (get-action-list)))))))
      (progn
        (format-header)
        (format t "~:{~&~2A ~3d ~4<~a~> ~3<~a~> ~A~}"
                (format-action-list :list-completed list-completed))
        (terpri)))))

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
          (when (and due (local-time:parse-timestring due :fail-on-error NIL))
            (if (getf updated-action :due)
                (setf (getf updated-action :due) due)
                (setf updated-action
                      (append updated-action (list :due due)))))
          (when (and wait (local-time:parse-timestring wait :fail-on-error NIL))
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

(defun log-action (description &key (priority "") (time-estimated 0))
  "Log action which is already completed, without creating it first,
then completing it. The purpose is to keep a log of actions undertaken
and completed, even if some of them weren't managed from within Action!"
  (progn
    (lazy-load-completed-actions)
    (let* ((timestamp (format-timestring 'NIL (now)))
           (uuid
             (intern (format nil "~s" (make-v4-uuid))))
           (completed-action
             (list :id uuid :priority priority
                   :time-estimated time-estimated :description description 
                   :created-on timestamp
                   :status "logged completed"
                   :logged-completed-on timestamp)))
      (and
       (append-to-activity-log completed-action :LOG-ACTION)
       (action/persistence:write-sexp-to-file
        +completed-actions-data-file+ completed-action
        :exists-action :append)
       (push completed-action *completed-actions-list*)
       uuid ))))

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
