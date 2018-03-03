(defpackage clw-tetris/src/game/score
  (:use :cl
        :ps-experiment
        :cl-ps-ecs
        :cl-web-2d-game)
  (:export :init-score-manager)
  (:import-from :clw-tetris/src/game/basic-operation
                :add-after-deleted-lines-hook))
(in-package :clw-tetris/src/game/score)

(defvar.ps+ *max-score* 99999)

(defun.ps+ get-score (score-manager)
  (get-entity-param score-manager :score))

(defun.ps+ update-score-text-area (score)
  (let ((area (find-a-entity-by-tag :score-text-area)))
    (clear-text-area area)
    (add-text-to-area area :text score :color #xffffff)))

(defun.ps+ make-score-text-area ()
  ;; TODO: Get screen width and height.
  (let ((area (make-text-area :font-size 30
                              :text-align :right
                              :margin 10
                              :x 800
                              :y 600)))
    (add-entity-tag area :score-text-area)
    area))

(defun.ps+ make-score-manager ()
  (let ((manager (make-ecs-entity)))
    (add-entity-tag manager :score-manager)
    (add-ecs-component-list
     manager
     (init-entity-params :score 0))
    manager))

(defun.ps+ add-score-after-deleted (num-lines)
  (let* ((manager (find-a-entity-by-tag :score-manager))
         (score (min *max-score*
                     (+ (get-score manager)
                        (* 1000 (expt 2 (1- num-lines)))))))
    (set-entity-param manager :score score)
    (update-score-text-area score)))

(defun.ps+ init-score-manager ()
  (let ((manager (make-score-manager)))
    (add-after-deleted-lines-hook #'add-score-after-deleted)
    (add-ecs-entity manager)
    (add-ecs-entity (make-score-text-area) manager)
    (update-score-text-area 0)))
