(in-package :cl-user)
(defpackage clw-tetris.game.mouse
  (:use :cl
        :ps-experiment
        :cl-ps-ecs
        :cl-web-2d-game)
  (:export :init-mouse-entity)
  (:import-from :clw-tetris.game.basic-operation
                :calc-global-piece-shape
                :piece
                :clone-piece
                :piece-x
                :piece-y))
(in-package :clw-tetris.game.mouse)

(defvar.ps+ *normal-block-frame-color* #x00ffff)
(defvar.ps+ *block-frame-depth* 20)

(defvar.ps+ *mouse-pointer-r* 3)
(defvar.ps+ *mouse-pointer-color* #xee0000)
(defvar.ps+ *mouse-pointer-depth* 30)

(defun.ps+ get-field-entity ()
  (find-a-entity-by-tag :field))

(defun.ps+ get-block-width (field-entity)
  (get-entity-param field-entity :block-width))

(defun.ps+ get-block-height (field-entity)
  (get-entity-param field-entity :block-height))

(defstruct.ps+ pnt-on-field x y)

(defun.ps+ init-pnt-on-field (field x y)
  (let ((x-count (get-entity-param field :x-count))
        (y-count (get-entity-param field :y-count)))
    (If (and (>= x 0) (< x x-count)
             (>= y 0) (< y y-count))
        (make-pnt-on-field :x x :y y)
        (make-pnt-on-field :x -99999 :y -99999))))

(defun.ps+ calc-mouse-point-on-field ()
  (let* ((mouse-x (get-mouse-x))
         (mouse-y (get-mouse-y))
         (field (get-field-entity))
         (block-width (get-block-width field))
         (block-height (get-block-height field)))
    (with-ecs-components ((field-pnt point-2d)) field
      (init-pnt-on-field field
                         (floor (/ (- mouse-x (point-2d-x field-pnt)) block-width))
                         (floor (/ (- mouse-y (point-2d-y field-pnt)) block-height))))))

(defun.ps+ make-block-frame-model ()
  (let* ((field (get-field-entity))
         (block-width (get-block-width field))
         (block-height (get-block-height field)))
    (make-model-2d :model (make-wired-rect :width block-width :height block-height
                                           :color *normal-block-frame-color*)
                   :offset (make-point-2d :x -99999 :y -99999)
                   :depth *block-frame-depth*)))

(defun.ps+ update-block-frame (mouse-entity)
  (let* ((models (get-entity-param mouse-entity :models))
         (field-entity (get-field-entity))
         (block-width (get-block-width field-entity))
         (block-height (get-block-height field-entity))
         (piece-entity (get-entity-param field-entity :current-piece)))
    (with-ecs-components (piece) piece-entity
      (let ((cloned-piece (clone-piece piece)))
        (with-slots (x y) (calc-mouse-point-on-field)
          (setf (piece-x cloned-piece) x
                (piece-y cloned-piece) y))
        (let ((shape (calc-global-piece-shape cloned-piece)))
          (with-ecs-components ((field-pnt point-2d)) field-entity
            (dotimes (i (length models))
              (let* ((model (nth i models))
                     (model-offset (model-2d-offset model))
                     (point (nth i shape)))
                (setf (point-2d-x model-offset)
                      (+ (* (car point) block-width) (point-2d-x field-pnt)))
                (setf (point-2d-y model-offset)
                      (+ (* (cadr point) block-height) (point-2d-y field-pnt)))))))))))

;; --- mouse pointer --- ;;
(defun.ps+ make-mouse-pointer-model ()
  (make-model-2d :model (make-solid-circle :r *mouse-pointer-r*
                                           :color *mouse-pointer-color*)
                 :depth *mouse-pointer-depth*))

(defun.ps+ update-mouse-pointer (entity)
  (let* ((model (get-entity-param entity :pointer-model))
         (model-offset (model-2d-offset model)))
    (assert model)
    (setf (point-2d-x model-offset) (get-mouse-x)
          (point-2d-y model-offset) (get-mouse-y))))

;; --- init --- ;;
(defun.ps+ init-mouse-entity ()
  (let ((entity (make-ecs-entity))
        (models '())
        (pointer-model (make-mouse-pointer-model)))
    (add-ecs-component-list
     entity
     (make-point-2d)
     pointer-model
     (make-script-2d :func (lambda (entity)
                             (update-block-frame entity)
                             (update-mouse-pointer entity)))
     (init-entity-params :models models
                         :pointer-model pointer-model))
    (dotimes (i 4)
      (let ((model (make-block-frame-model)))
        (push model models)
        (add-ecs-component model entity)))
    (add-ecs-entity entity)))
