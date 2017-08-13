#|
  This file is a part of stage project.
  Copyright (c) 2017 Sacha El Masry
|#

(in-package :cl-user)
(defpackage stage-test-asd
  (:use :cl :asdf))
(in-package :stage-test-asd)

(defsystem stage-test
  :author "Sacha El Masry"
  :license "BSD"
  :depends-on (:stage
               :prove)
  :components ((:module "t"
                :components
                ((:test-file "stage"))))
  :description "Test system for stage"

  :defsystem-depends-on (:prove-asdf)
  :perform (test-op :after (op c)
                    (funcall (intern #.(string :run-test-system) :prove-asdf) c)
                    (asdf:clear-system c)))
