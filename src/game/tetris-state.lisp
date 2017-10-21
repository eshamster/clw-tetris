(in-package :cl-user)
(defpackage clw-tetris.game.tetris-state
  (:use :cl
        :ps-experiment
        :cl-ps-ecs
        :cl-web-2d-game
        :clw-tetris.game.game-state
        :clw-tetris.game.entity)
  (:export :make-tetris-start-state))
(in-package :clw-tetris.game.tetris-state)

(defstruct.ps+ (tetris-main-state
                (:include game-state)))

(defstruct.ps+ (tetris-start-state
                (:include game-state
                          (start-process
                           (lambda ()
                             (LET* ((field-entity (make-field-entity
                                                   :x-count 10
                                                   :y-count 10))
                                    (field (get-ecs-component 'field field-entity)))
                               (assert field)
                               (add-ecs-entity field-entity)
                               (add-ecs-entity (make-piece-entity field)))
                             t))
                          (process (lambda () nil)))))
