(in-package :cl-user)
(defpackage clw-tetris.game-state
  (:use :cl
        :ps-experiment
        :cl-ps-ecs
        :cl-web-2d-game)
  (:export :init-func
           :update-func))
(in-package :clw-tetris.game-state)

