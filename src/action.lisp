(in-package :cl-user)
(defpackage action
  (:nicknames :act :ac)
  (:use :cl)
  (:import-from :uuid
   :make-v4-uuid)
  (:export
   :add-action
   :list-actions
   :delete-action
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

(defun list-actions (&key (action-list *action-list*)
                       (completed-actions-list *completed-actions-list*)
                       list-completed)
  (if list-completed
      (get-completed-actions-list)
      (get-action-list)))

(defun delete-action ()
  ())

(defun edit-action ()
  ())

(defun complete-action ()
  ())
