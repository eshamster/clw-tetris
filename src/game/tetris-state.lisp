(in-package :cl-user)
(defpackage clw-tetris.game.tetris-state
  (:use :cl
        :ps-experiment
        :cl-ps-ecs
        :cl-web-2d-game
        :clw-tetris.game.game-state)
  (:export :make-tetris-start-state))
(in-package :clw-tetris.game.tetris-state)

(defstruct.ps+ (tetris-main-state
                (:include game-state)))

(defstruct.ps+ (tetris-start-state
                (:include game-state
                          (process (lambda () (make-tetris-start-state))))))
