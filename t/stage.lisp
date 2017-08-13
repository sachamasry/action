(in-package :cl-user)
(defpackage stage-test
  (:use :cl
        :stage
        :prove))
(in-package :stage-test)

;; NOTE: To run this test file, execute `(asdf:test-system :stage)' in your Lisp.

(plan nil)

;; blah blah blah.

(finalize)
