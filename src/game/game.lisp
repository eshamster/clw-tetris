(in-package :cl-user)
(defpackage clw-tetris.game
  (:use :cl
        :ps-experiment
        :cl-ps-ecs
        :cl-web-2d-game)
  (:export :init-func
           :update-func))
(in-package :clw-tetris.game)

(defun.ps+ init-func (scene)
  (init-default-systems :scene scene))

(defun.ps+ update-func ()
  (do-ecs-entities entity
    (add-to-monitoring-log (ecs-entity-id entity))))
