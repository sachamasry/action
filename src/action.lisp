(in-package :cl-user)
(defpackage action
  (:nicknames :act :ac)
  (:use :cl)
  (:import-from :uuid
                :uuid=
   :make-v4-uuid)
  (:import-from :alexandria
                :when-let)
  (:export
   :add-action
   :cli-list-actions
   :remove-action
   :edit-action
   :complete-action))
(in-package :action)

(defvar *action-list* ())

(defun set-action-list (new-value)
  (setf *action-list* new-value))

(defun add-action-to-action-list (action)
  (push action *action-list*))

(defun get-action-list ()
  *action-list*)

(defvar *completed-actions-list* ())

(defun add-action-to-completed-actions-list (action)
  (push action *completed-actions-list*))

(defun get-completed-actions-list ()
  *completed-actions-list*)

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
