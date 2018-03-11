(defpackage clw-tetris/src/game/game
  (:use :cl
        :ps-experiment
        :cl-ps-ecs
        :cl-web-2d-game
        :clw-tetris/src/game/tetris-state)
  (:export :init-func
           :update-func))
(in-package :clw-tetris/src/game/game)

(defun.ps+ init-func (scene)
  (init-game-state (make-tetris-start-state))
  (init-default-systems :scene scene)
  (init-input))

(defun.ps+ update-func ()
  (process-game-state))
