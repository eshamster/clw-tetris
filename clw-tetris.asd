#|
  This file is a part of clw-tetris project.
  Copyright (c) 2017 eshamster (hamgoostar@gmail.com)
|#

#|
  Sample tetris written in Common Lisp for web

  Author: eshamster (hamgoostar@gmail.com)
|#

(in-package :cl-user)
(defpackage clw-tetris-asd
  (:use :cl :asdf))
(in-package :clw-tetris-asd)

(defsystem clw-tetris
  :version "0.1"
  :author "eshamster"
  :license "LLGPL"
  :depends-on (:parenscript
               :ps-experiment
               :cl-ps-ecs
               :cl-web-2d-game
               :ningle
               :cl-markup
               :clack)
  :components ((:module "src/game"
                :serial t
                :components
                ((:file "basic-operation")
                 (:file "game-state")
                 (:file "tetris-state")
                 (:file "game")))
               (:module "src"
                :serial t
                :components
                ((:file "server")
                 (:file "clw-tetris"))
                :depends-on ("src/game")))
  :description "Sample tetris written in Common Lisp for web"
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
  :in-order-to ((test-op (test-op clw-tetris-test))))
