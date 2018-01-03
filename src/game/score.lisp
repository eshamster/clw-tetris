(in-package :cl-user)
(defpackage clw-tetris.game.score
  (:use :cl
        :ps-experiment
        :cl-ps-ecs
        :cl-web-2d-game)
  (:export :init-score-manager)
  (:import-from :clw-tetris.game.basic-operation
                :add-after-deleted-lines-hook))
(in-package :clw-tetris.game.score)

(defvar.ps+ *max-score* 99999)

(defun.ps+ get-score (score-manager)
  (get-entity-param score-manager :score))

(defun.ps+ make-score-manager ()
  (let ((manager (make-ecs-entity)))
    (add-entity-tag manager :score-manager)
    (add-ecs-component-list
     manager
     (init-entity-params :score 0))
    manager))

(defun.ps+ add-score-after-deleted (num-lines)
  (let* ((manager (find-a-entity-by-tag :score-manager))
         (new-score (+ (get-score manager)
                       (* 1000 (expt 2 (1- num-lines))))))
    (set-entity-param manager :score
                      (if (< new-score *max-score*) new-score *max-score*))
    (add-to-event-log (get-score manager))))

(defun.ps+ init-score-manager ()
  (let ((manager (make-score-manager)))
    (add-after-deleted-lines-hook #'add-score-after-deleted)
    (add-ecs-entity manager)))
