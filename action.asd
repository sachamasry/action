#|
  This file is a part of Action! project.
  Copyright (c) 2017 Sacha El Masry
|#

#|
  Action! is a simple To-Do list manager

  Author: Sacha El Masry
|#

(in-package :cl-user)
(defpackage action-asd
  (:use :cl :asdf))
(in-package :action-asd)

(defsystem action
  :version "0.1"
  :author "Sacha El Masry"
  :license "BSD"
  :depends-on (:alexandria
               :cl-strings
               :local-time
               :uuid 
               :cl-ppcre
               :ratify)
  :components ((:module "src"
                :components
                        ((:file "filesystem-interface")
                         (:file "string-manipulation")
                         (:file "action"))))
  :description "Action! is a simple To-Do list manager"
  :long-description
  #.(with-open-file (stream (merge-pathnames
                             #p"README.markdown"
                             (or *load-pathname* *compile-file-pathname*))
                            :if-does-not-exist nil
                            :direction :input)
      (when stream
        (let ((seq (make-array (file-length stream)
                               :element-type 'character
                               :fill-pointer t)))
          (setf (fill-pointer seq) (read-sequence seq stream))
          seq)))
  :in-order-to ((test-op (test-op action-test))))
