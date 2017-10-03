(in-package :cl-user)

(defpackage action/cli
  (:use :cl))
(in-package :action/cli)

(require "asdf")
(load "bundle")
(asdf:load-system :net.didierverna.clon)
(asdf:load-system :action)
(asdf:load-system :cl-strings)
(use-package :net.didierverna.clon)
(use-package :action)

(defsynopsis (:postfix "cmd [OPTIONS]")
  (text :contents "Action! is a simple to-do list.

Available commands: add log edit append prepend delete complete

Use 'cmd --help to get command-specific help.")
  (group (:header "Immediate exit options:")
         (flag :short-name "h" :long-name "help"
               :description "Print this help and exit.")
         (flag :short-name "v" :long-name "version"
               :description "Print version and exit.")))

(defconstant +add-synopsis+
  (defsynopsis (:make-default nil)
    (text :contents "Add new pending action.")
    (flag :short-name "h" :long-name "help"
          :description "add [ description ]"))
  "The 'add' command will create a new pending action.")

(defun main ()
  "Entry point for the application"
  (make-context)
  (cond ((getopt :short-name "h")
         (help))
        ((getopt :short-name "v")
         (format t "Action! version ~s~%"
                 (action:system-version :action)))
        (t
         (unless (remainder)
           (action:cli-list-actions)
           (exit))
         (cond
           ;; list outstanding actions
           ((or (string= (first (remainder)) "list")
                (string= (first (remainder)) "ls"))
            (action:cli-list-actions))

           ;; complete action
           ((and (or (string= (first (remainder)) "done")
                     (string= (first (remainder)) "complete")
                     (string= (first (remainder)) "completed")
                     (string= (first (remainder)) "comp"))
                 (second (remainder)))
            (and
             (action:complete-action
              (string-upcase (second (remainder))))
             (format t "Activity ~a completed.~%" (second (remainder)))))

           ;; delete action
           ((and
             (or (string= (first (remainder)) "delete")
                 (string= (first (remainder)) "del")
                 (string= (first (remainder)) "remove")
                 (string= (first (remainder)) "rem")
                 (string= (first (remainder)) "rm"))
             (second (remainder)))
            (and
             (action:delete-action (second (remainder)))
             (format t "Activity ~a deleted.~%" (second (remainder)))))

           ;; log action
           ((string= (first (remainder)) "log")
            (and
             (action:log-action
              (cl-strings:join (rest (remainder)) :separator " "))
             (format t "Logged completed activity.~%")))

           ;; edit action
           ((and (or (string= (first (remainder)) "edit")
                     (string= (first (remainder)) "ed"))
                 (second (remainder))
                 (third (remainder)))
            (and
             (action:edit-action
              (string-upcase (second (remainder)))
              (cl-strings:join 
               (rest (rest (remainder))) :separator " "))
             (format t "Activity ~a updated.~%" (second (remainder)))))

           ;; prepend
           ((and (or (string= (first (remainder)) "prepend")
                     (string= (first (remainder)) "pre"))
                 (second (remainder))
                 (third (remainder)))
            (and
             (action:prepend-to-action
              (string-upcase (second (remainder)))
              (cl-strings:join 
               (rest (rest (remainder))) :separator " "))
             (format t "Activity ~a updated.~%" (second (remainder)))))

           ;; append
           ((and (or (string= (first (remainder)) "append")
                     (string= (first (remainder)) "app")
                     (string= (first (remainder)) "ap"))
                 (second (remainder))
                 (third (remainder)))
            (and
             (action:append-to-action
              (string-upcase (second (remainder)))
              (cl-strings:join 
               (rest (rest (remainder))) :separator " "))
             (format t "Activity ~a updated.~%" (second (remainder)))))

           ;; If there is a remainder, and if the first item is
           ;; "add" then create new activity using the rest of the
           ;; remainder
           ;; Otherwise, create new activity using the remainder
           (t
            (cond ((string= (first (remainder)) "add")
                   (and
                    (action:add-action
                     (cl-strings:join (rest (remainder)) :separator " "))
                    (format t "New activity added.~%")))
                  (t
                   (and
                    (action:add-action
                     (cl-strings:join (remainder) :separator " "))
                    (format t "New activity added.~%"))))))
         (exit))))
               
         ;; (make-context
         ;;  :synopsis (cond ((string= (first (remainder)) "add")
         ;;                   +add-synopsis+)
         ;;                  (t
         ;;                   (format t "Unknown command.~%")
         ;;                   (exit 1)))
         ;;  :cmdline (print (cl-strings:join (remainder) :separator " ")))
         ;; (cond ((getopt :short-name "h")
         ;;        (help))
         ;;       (t
         ;;        (format t "Command name: ~A~%~%" (progname))
         ;;        (format t "Options:")
         ;;        (do-cmdline-options (option name value source)
         ;;          (print (list option name value source)))
         ;;        (terpri)
         ;;        (format t "Remainder: ~A~%" (remainder))))))

(dump "act" main)
