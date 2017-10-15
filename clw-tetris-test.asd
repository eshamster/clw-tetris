#|
  This file is a part of clw-tetris project.
  Copyright (c) 2017 eshamster (hamgoostar@gmail.com)
|#

(in-package :cl-user)
(defpackage clw-tetris-test-asd
  (:use :cl :asdf))
(in-package :clw-tetris-test-asd)

(defsystem clw-tetris-test
  :author "eshamster"
  :license "LLGPL"
  :depends-on (:clw-tetris
               :prove)
  :components ((:module "t"
                :components
                ((:test-file "clw-tetris"))))
  :description "Test system for clw-tetris"

  :defsystem-depends-on (:prove-asdf)
  :perform (test-op :after (op c)
                    (funcall (intern #.(string :run-test-system) :prove-asdf) c)
                    (asdf:clear-system c)))
