(defpackage clw-tetris/src/game/tetris-state
  (:use :cl
        :ps-experiment
        :cl-ps-ecs
        :cl-web-2d-game
        :clw-tetris/src/game/entity)
  (:export :make-tetris-start-state)
  (:import-from :clw-tetris/src/game/mouse
                :init-mouse-entity)
  (:import-from :clw-tetris/src/game/score
                :init-score-manager))
(in-package :clw-tetris/src/game/tetris-state)

(defstruct.ps+
    (tetris-main-state
     (:include game-state
               (start-process
                (lambda (_this)
                  (declare (ignore _this))
                  (init-tetris-entities)
                  (init-mouse-entity)
                  (init-score-manager)
                  t))
               (process
                (lambda (_this)
                  (declare (ignore _this))
                  (when (gameover-p (find-a-entity-by-tag :field))
                    (make-tetris-gameover-state)))))))

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

(defstruct.ps+
    (tetris-gameover-state
     (:include game-state
               (start-process
                (lambda (_this)
                  (declare (ignore _this))
                  (let* ((font-size 35)
                         (margin 20)
                         (area (make-text-area :font-size font-size :text-align :center
                                               :margin margin
                                               :x (/ (get-screen-width) 2)
                                               :y (+ (/ (get-screen-height) 2)
                                                     (+ (* font-size 2) margin)))))
                    ;; TODO: Make key name configurable
                    (add-text-to-area area :text "GAME OVER!!" :color #xff0000)
                    (add-text-to-area area
                                      :text "Press z key or Click to restart"
                                      :color #x00ffff)
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
                  (with-slots (first-frame) _this
                    (if first-frame
                        (progn (do-ecs-entities entity
                                 (unless (ecs-entity-parent entity)
                                   (register-next-frame-func
                                    (lambda () (delete-ecs-entity entity)))))
                               (setf first-frame nil)
                               nil)
                        t))))))
    (first-frame t))
