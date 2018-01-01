(in-package :cl-user)
(defpackage clw-tetris.game
  (:use :cl
        :ps-experiment
        :cl-ps-ecs
        :cl-web-2d-game
        :clw-tetris.game.game-state
        :clw-tetris.game.tetris-state)
  (:export :init-func
           :update-func))
(in-package :clw-tetris.game)

(defun.ps+ init-func (scene)
  (init-game-state (make-tetris-start-state))
  (init-default-systems :scene scene)
  (init-input))

(defun.ps+ update-func ()
  (process-game-state)
  (do-ecs-entities entity
    (add-to-monitoring-log (ecs-entity-id entity))))
