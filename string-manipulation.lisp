(in-package :cl-user)
(defpackage :action/string-manipulation
  (:nicknames :astr)
  (:use :cl)
  (:import-from :alexandria
   :when-let)
  (:import-from :cl-strings
   :join
                :split
   :clean
                :camel-case
   :kebab-case
                :snake-case)
  (:export #:string->snake-case
           #:string-escape))
(in-package :action/string-manipulation)

#|
STRING MANIPULATION

A series of utility functions changing strings to different formats, as needed for
command-line use, or for configuration files with different requirements of snake-,
kebab-, camel-case or other string representation
|#

(defun string->snake-case (string &key case)
  "Returns STRING in snake-case"
  (snake-case (join (split string "-") :separator " ")))

(defun string-escape (string)
  "Protect a string by escaping it in another layer of quotes."
  (concatenate 'string "\"" string "\""))
