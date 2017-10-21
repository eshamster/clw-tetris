(in-package :cl-user)
(defpackage clw-tetris.game.entity
  (:use :cl
        :ps-experiment
        :cl-ps-ecs
        :cl-web-2d-game
        :clw-tetris.game.basic-operation)
  (:export :make-piece-entity
           :make-field-entity))
(in-package :clw-tetris.game.entity)

;; --- moving --- ;;

(defun.ps+ down-piece-entity (entity &key piece field)
  (unless piece
    (setf piece (get-ecs-component 'piece entity)))
  (unless field
    (setf field (get-entity-param entity :field)))
  (assert piece)
  (assert field)
  (set-entity-param entity :rest-intv
                    (get-entity-param entity :intv))
  (unless (move-piece-to field piece :down)
    (register-next-frame-func (lambda ()
                                (delete-ecs-entity entity)))
    (if (pin-piece-to-field field piece)
        (add-ecs-entity-to-buffer (make-piece-entity field))
        (set-entity-param field :gameover-p t))))

;; TODO: Implement continuous move when continuing to press key
;; TODO: Rotate piece
(defun.ps+ process-tetris-input (entity)
  (with-ecs-components (piece) entity
    (let ((field (get-entity-param entity :field)))
      (labels ((require-move-p (key-name)
                 (is-key-down-now key-name))
               (move-if-required (key-name move-direction)
                 (when (require-move-p key-name)
                   (move-piece-to field piece move-direction))))
        (move-if-required :left :left)
        (move-if-required :right :right)
        (when (require-move-p :down)
          (down-piece-entity entity :piece piece :field field))))))

(defun.ps+ fall-in-natural (entity)
  (when (<= (set-entity-param entity :rest-intv
                              (1- (get-entity-param entity :rest-intv)))
            0)
    (down-piece-entity entity)))

;; --- drawing --- ;;

(defun.ps+ update-field-draw (field-entity)
  (check-type field-entity ecs-entity)
  (let ((block-model-array (get-entity-param field-entity :block-model-array))
        (field (get-ecs-component 'field field-entity)))
    (assert field)
    ;; draw static state
    (loop for i from 0 below (length block-model-array)
       do (if (aref (field-block-state-array field) i)
              (enable-model-2d field-entity
                               :target-model-2d (aref block-model-array i))
              (disable-model-2d field-entity
                                :target-model-2d (aref block-model-array i))))
    ;; draw pieces
    (do-ecs-entities entity
      (when (has-entity-tag entity :piece)
        (with-ecs-components (piece) entity
          (let ((shape (calc-global-piece-shape piece)))
            (dolist (point shape)
              (let ((index (+ (car point)
                              (* (cadr point) (field-x-count field)))))
                (assert (< index (* (field-x-count field)
                                    (field-y-count field))))
                (enable-model-2d field-entity
                                 :target-model-2d (aref block-model-array index))))))))))

;; --- main --- ;;

(defun.ps+ update-field (field-entity)
  (update-field-draw field-entity)
  (do-ecs-entities entity
    (when (has-entity-tag entity :piece)
      (process-tetris-input entity)
      (fall-in-natural entity))))

(defun.ps+ make-piece-entity (field)
  (let ((entity (make-ecs-entity)))
    (add-entity-tag entity :piece)
    (add-ecs-component-list
     entity
     (init-piece field))
    entity))

(defun.ps+ make-field-entity (&key x-count y-count)
  (let ((field (init-tetris-field x-count y-count))
        (block-model-array (make-array (* x-count y-count)))
        (entity (make-ecs-entity))
        ;; TODO: consider block parameters
        (block-width 20)
        (block-height 20)
        (block-margin 2))
    (dotimes (i (* x-count y-count))
      (let* ((x (mod i x-count))
             (y (/ i x-count))
             (model (make-model-2d
                     :model (make-solid-rect :width (- block-width (* block-margin 2))
                                             :height (- block-height (* block-height 2))
                                             :color #xeeeeee)
                     :offset (make-point-2d :x (+ (* block-width x) block-margin)
                                            :y (+ (* block-height y) block-margin)))))
        (setf (aref block-model-array i) model)
        (add-ecs-component model entity)))
    (add-ecs-component-list
     entity
     field
     ;; TODO: consider point (offset)
     (make-point-2d :x 10 :y 10)
     (init-entity-params :block-model-array block-model-array
                         :gameover-p nil)
     (make-script-2d :func #'update-field))
    entity))
