(in-package :cl-user)
(defpackage action
  (:nicknames :act :ac)
  (:use :cl)
  (:import-from :local-time
                #:now
                #:format-timestring)
  (:import-from :uuid
                #:uuid=
   :make-v4-uuid)
  (:import-from :alexandria
                #:when-let)
  (:import-from :action/persistence
                #:write-sexp-to-file
                #:read-sexp-from-file)
  (:export
   #:system-version

   #:add-action
   #:cli-list-actions
   #:delete-action
   #:edit-action
   #:prepend-to-action
   #:append-to-action
   #:complete-action
   #:log-action))
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

;; Ensure the directory exists, creating if if necessary
(defparameter +action-data-directory+ 
  (ensure-directories-exist
   (action/filesystem-interface:construct-directory 
    (uiop/configuration:xdg-data-home) "action")))

(defparameter +actions-data-file+
  (merge-pathnames +action-data-directory+ "actions.data"))

(defparameter +completed-actions-data-file+
  (merge-pathnames +action-data-directory+ "completed.data"))

(defparameter +activity-log+ 
  (merge-pathnames +action-data-directory+ "activity-log.data"))

;; Ensure all required data files exist, creating empty files if
;; necessary
(defun ensure-file-exists (file)
  (unless (probe-file file)
    (with-open-file (s file :direction :output)
      (write-string "" s))))

(defun ensure-files-exist (&rest files)
  (mapcar #'(lambda (file) (ensure-file-exists file))
          files))

(ensure-files-exist +actions-data-file+
                    +completed-actions-data-file+
                    +activity-log+)

;; Create *action-list* variable, holding a list of next actions
;; Load persisted actions from file upon first definition
(defvar *action-list*
  (with-open-file (file +actions-data-file+ :direction :input)
    (with-standard-io-syntax
      (let ((*read-eval* nil))
        (setf *action-list* (read file 'NIL 'NIL))))))

(defun get-action-list ()
  *action-list*)

;; Upon every setting of the action list, persist the list to file
(defun set-action-list (new-value)
  (action/persistence:write-sexp-to-file +actions-data-file+
                                         (setf *action-list* new-value)))

(defun add-action-to-action-list (action)
  (and
   (append-to-activity-log action "create new action")
   (action/persistence:write-sexp-to-file +actions-data-file+
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
       (append-to-activity-log completed-action "completed action"
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
  (let ((activity-to-log
          (cond (old-action (list :time (format-timestring 'NIL (now))
                                  :action-id (getf action :id)
                                  :activity activity-type
                                  :from old-action
                                  :to action))
                (t (list :time (format-timestring 'NIL (now))
                         :action-id (getf action :id)
                         :activity activity-type
                         action)))))
    (action/persistence:write-sexp-to-file
     +activity-log+ activity-to-log
     :exists-action :append)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Define main action verbs
(defun add-action (description &key (priority "") (time-estimated 0))
  (let ((timestamp (format-timestring 'NIL (now))))
    (add-action-to-action-list
     (list :id (make-v4-uuid) :priority priority
           :time-estimated time-estimated :description description 
           :created-on timestamp)))) 

(defun action-list (&key (action-list *action-list*)
                       (completed-actions-list *completed-actions-list*)
                       list-completed)
  (if list-completed
      (get-completed-actions-list)
      (get-action-list)))

(defun shorten-id (id digits)
  (subseq (format nil "~s" id) 0 digits))

(defun get-id-from-fragment (short-id &key (action-list *action-list*)
                                      (short-id-length 2))
  (when (and short-id
             (or (stringp short-id)
                 (setf short-id (format nil "~a" short-id))))
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

(defun cli-list-actions (&key (action-list *action-list*)
                       (completed-actions-list *completed-actions-list*)
                       list-completed)
  (flet ((format-header ()
           (format t "ID Priority Time Description~%")
           (format t "-- -------- ---- -----------~%"))
         (format-action-list (&key list-completed)
           (mapcar
            #'(lambda (action) (list (shorten-id (getf action :id) 2)
                                   (getf action :priority)
                                   (getf action :time-estimated)
                                   (getf action :description)))
            (if list-completed (get-completed-actions-list)
                (get-action-list)))))
    (progn
      (format-header)
      (format t "~:{~&~2A ~8A ~4D ~A~}"
              (format-action-list :list-completed list-completed)))))

(defun get-action-by-id (id &key (action-list *action-list*))
  (when id
    (when-let ((expanded-id (get-id-from-fragment id)))
      (find-if
       #'(lambda (action) (uuid=
                           expanded-id
                           (getf action :id)))
     action-list))))

(defun remove-action (id &key (action-list *action-list*))
  (when id
    (when-let ((matching-action (get-action-by-id id)))
      (set-action-list
       (remove-if
        #'(lambda (action) (eq action matching-action))
        action-list)))))

(defun delete-action (id &key (reason ""))
  (when id
    (when-let ((matching-action (get-action-by-id id)))
      (let ((deleted-action
              (append matching-action
                      (list :status "deleted"
                            :reason reason
                            :deleted-on
                            (format-timestring 'NIL (now))))))
        (and
         (append-to-activity-log deleted-action "delete action"
                                 :old-action matching-action)
         (remove-action id))))))

(defun edit-action (id description &key merge-with-description
                                     priority time-estimated)
  (when id
    (when-let ((matching-action (get-action-by-id id)))
      (let ((updated-action (copy-list matching-action)))
        (when (not (zerop (length description)))
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
        (if (getf updated-action :modified-on)
            (setf (getf updated-action :modified-on)
                  (format-timestring 'NIL (now)))
            (setf updated-action
                  (append updated-action
                          (list :status "modified"
                                :modified-on
                                (format-timestring 'NIL (now))))))
        (and
         (append-to-activity-log updated-action "update action"
                                 :old-action matching-action)
         (set-action-list
          (mapcar #'(lambda (action) (if 
                                      (eq (get-action-by-id id) action)
                                      updated-action
                                      action))
                  (get-action-list))))))))

(defun prepend-to-action (id text)
  (when (and id
             (plusp (length text)))
    (edit-action id text :merge-with-description :prepend)))

(defun append-to-action (id text)
  (when (and id
             (plusp (length text)))
    (edit-action id text :merge-with-description :append)))

(defun complete-action (id)
  (when id
    (when-let ((matching-action (get-action-by-id id)))
      (and
       (add-action-to-completed-actions-list matching-action)
       (remove-action id)))))

(defun log-action (description &key (priority "") (time-estimated 0))
  "Log action which is already completed, without creating it first,
then completing it. The purpose is to keep a log of actions undertaken
and completed, even if some of them weren't managed from within Action!"
  (progn
    (lazy-load-completed-actions)
    (let* ((timestamp (format-timestring 'NIL (now)))
           (completed-action
             (list :id (make-v4-uuid) :priority priority
                   :time-estimated time-estimated :description description 
                   :created-on timestamp
                   :status "logged completed"
                   :logged-completed-on timestamp)))
      (and
       (append-to-activity-log completed-action "log action")
       (action/persistence:write-sexp-to-file
        +completed-actions-data-file+ completed-action
        :exists-action :append)
       (push completed-action *completed-actions-list*)
       t))))

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
