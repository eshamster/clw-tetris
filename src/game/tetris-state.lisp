(in-package :cl-user)
(defpackage clw-tetris.game.tetris-state
  (:use :cl
        :ps-experiment
        :cl-ps-ecs
        :cl-web-2d-game
        :clw-tetris.game.game-state
        :clw-tetris.game.entity)
  (:export :make-tetris-start-state)
  (:import-from :clw-tetris.game.mouse
                :init-mouse-entity))
(in-package :clw-tetris.game.tetris-state)

(defstruct.ps+
    (tetris-main-state
     (:include game-state
               (start-process
                (lambda (_this)
                  (declare (ignore _this))
                  (init-tetris-entities)
                  (init-mouse-entity)
                  t)))))

(defstruct.ps+
    (tetris-start-state
     (:include game-state
               (start-process
                (lambda (_this)
                  ;; TODO: Prevent multiple load
                  (load-font "js/")
                  (let ((area (make-text-area :font-size 25 :text-align :center
                                              :x 400 :y 300)))
                    (setf (tetris-start-state-text-area-entity _this)
                          area)
                    ;; TODO: Make key name configurable
                    (add-text-to-area area
                                      :text "Press z key or Click to start"
                                      :color #xffffff)
                    (add-ecs-entity area))
                  t))
               (process
                (lambda (_this)
                  (declare (ignore _this))
                  (when (or (is-key-down-now :a)
                            (eq (get-left-mouse-state) :down-now))
                    (make-tetris-main-state))))
               (end-process
                (lambda (_this)
                  (delete-ecs-entity (tetris-start-state-text-area-entity _this))
                  t))))
    text-area-entity)
