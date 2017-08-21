(in-package :cl-user)

(defpackage action/cli
  (:use :cl))
(in-package :action/cli)

(require "asdf")
(asdf:load-system :net.didierverna.clon)
(asdf:load-system :action)
(use-package :net.didierverna.clon)
(use-package :action)

(defsynopsis (:postfix "FILES...")
  (text :contents "Action! is a simple to-do list")
  (group (:header "Immediate exit options:")
         (flag :short-name "h" :long-name "help"
               :description "Print this help and exit.")
         (flag :short-name "v" :long-name "version"
               :description "Print version and exit.")))

(defun main ()
  "Entry point for the application"
  (make-context)
  (when (getopt :short-name "h")
    (help)
    (exit))
  (do-cmdline-options (option name value source)
    (print (list option name value source)))
  (terpri)
  (exit))

(dump "act" main)
