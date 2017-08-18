(in-package :cl-user)
(defpackage action
  (:nicknames :act :ac)
  (:use :cl)
  (:import-from :uuid
                :uuid=
   :make-v4-uuid)
  (:import-from :alexandria
                :when-let)
  (:import-from :action/persistence
                #:write-sexp-to-file
                #:read-sexp-from-file)
  (:export
   :add-action
   :cli-list-actions
   :remove-action
   :edit-action
   :complete-action))
(in-package :action)

;; Action! will store its data in an XDG compliant data directory,
;; $HOME/.config/action/
;; ensure the directory exists?
(defconstant +action-data-directory+ 
  (ensure-directories-exist
   (action/filesystem-interface:construct-directory 
    (uiop/configuration:xdg-data-home) "action")))

(defconstant +actions-data-file+
  (merge-pathnames +action-data-directory+ "actions.data"))

(defconstant +completed-actions-data-file+
  (merge-pathnames +action-data-directory+ "completed.data"))

(defconstant +activity-log+ 
  (merge-pathnames +action-data-directory+ "activity-log.data"))

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

(defvar *action-list*
  (action/persistence:read-sexp-from-file +actions-data-file+))

(defun get-action-list ()
  *action-list*)

(defun set-action-list (new-value)
  (action/persistence:write-sexp-to-file +actions-data-file+
                                         (setf *action-list* new-value)))

(defun add-action-to-action-list (action)
  (action/persistence:write-sexp-to-file +actions-data-file+
                                         (push action *action-list*)))

(defvar *completed-actions-list* ())

(defun get-completed-actions-list ()
  *completed-actions-list*)

(defun lazy-load-completed-actions ()
  (when (null (get-completed-actions-list))
    (setf *completed-actions-list*
          (action/persistence:read-sexp-from-file
           +completed-actions-data-file+))))

(defun add-action-to-completed-actions-list (action)
  (and
   (action/persistence:write-sexp-to-file
    +completed-actions-data-file+ action :exists-action :append)
   (push action *completed-actions-list*)))

(defun add-action (description &key (priority "") (time-estimated 0)
                                 (action-list *action-list*))
  (let ((timestamp (get-universal-time)))
    (add-action-to-action-list
     (list :priority priority :time-estimated time-estimated
           :created-on timestamp :modified-on timestamp
           :description description :uuid (make-v4-uuid)))))

(defun action-list (&key (action-list *action-list*)
                       (completed-actions-list *completed-actions-list*)
                       list-completed)
  (if list-completed
      (get-completed-actions-list)
      (get-action-list)))

(defun shorten-id (id digits)
  (subseq (format nil "~s" id) 0 digits))

(defun get-uuid-from-short-id (short-id &key (action-list *action-list*)
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
                                               (getf action :uuid))
                                       0 short-id-length)
                                      (getf action :uuid)))
               action-list))))))

(defun cli-list-actions (&key (action-list *action-list*)
                       (completed-actions-list *completed-actions-list*)
                       list-completed)
  (flet ((format-header ()
           (format t "ID Priority Time Description~%")
           (format t "-- -------- ---- -----------~%"))
         (format-action-list (&key list-completed)
           (mapcar
            #'(lambda (action) (list (shorten-id (getf action :uuid) 2)
                                   (getf action :priority)
                                   (getf action :time-estimated)
                                   (getf action :description)))
            (if list-completed completed-actions-list action-list))))
    (progn
      (format-header)
      (format t "~:{~&~2A ~8A ~4D ~A~}"
             (format-action-list :list-completed list-completed)))))

(defun get-action-by-id (id &key (action-list *action-list*))
  (when id
    (when-let ((expanded-uuid (get-uuid-from-short-id id)))
      (find-if
       #'(lambda (action) (uuid:uuid=
                         expanded-uuid
                         (getf action :uuid)))
     action-list))))

(defun remove-action (id &key (action-list *action-list*))
  (when id
    (when-let ((matching-action (get-action-by-id id)))
      (set-action-list
       (remove-if
        #'(lambda (action) (eq action matching-action))
        action-list)))))

(defun delete-action ()
  ())

(defun edit-action ()
  ())

(defun complete-action (id &key (action-list *action-list*)
                             (completed-actions-list *completed-actions-list*))
  (when id
    (when-let ((matching-action (get-action-by-id id)))
      (and
       (add-action-to-completed-actions-list matching-action)
       (remove-action id)))))

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
