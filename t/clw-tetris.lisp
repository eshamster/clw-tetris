(in-package :cl-user)
(defpackage clw-tetris-test
  (:use :cl
        :clw-tetris
        :prove))
(in-package :clw-tetris-test)

(plan 1)

(defvar *port* 21464)

;; Only test connection
(unwind-protect
     (progn
       (clw-tetris:start :port *port*)
       (handler-case
           (ok (dex:get (format nil "http://localhost:~D" *port*)))
         (error (e)
           (fail (format nil "~A" e)))))
  (clw-tetris:stop))

(finalize)
