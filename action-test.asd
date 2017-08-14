#|
  This file is a part of the Action! project.
  Copyright (c) 2017 Sacha El Masry
|#

(in-package :cl-user)
(defpackage action-test-asd
  (:use :cl :asdf))
(in-package :action-test-asd)

(defsystem action-test
  :author "Sacha El Masry"
  :license "BSD"
  :depends-on (:action
               :prove)
  :components ((:module "t"
                :components
                ((:test-file "action"))))
  :description "Test system for action"

  :defsystem-depends-on (:prove-asdf)
  :perform (test-op :after (op c)
                    (funcall (intern #.(string :run-test-system) :prove-asdf) c)
                    (asdf:clear-system c)))
