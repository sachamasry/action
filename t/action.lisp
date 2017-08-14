(in-package :cl-user)
(defpackage action-test
  (:use :cl
        :action
        :prove))
(in-package :action-test)

;; NOTE: To run this test file, execute `(asdf:test-system :action)' in your Lisp.

(plan nil)

;; blah blah blah.

(finalize)
