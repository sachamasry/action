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
         (cond ((string= (first (remainder)) "add")
                (action:add-action (remainder))))))
               
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
  (exit))

(dump "act" main)
