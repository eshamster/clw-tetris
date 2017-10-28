(in-package :cl-user)
(defpackage clw-tetris.game.entity
  (:use :cl
        :ps-experiment
        :cl-ps-ecs
        :cl-web-2d-game
        :clw-tetris.game.basic-operation)
  (:export :init-tetris-entities))
(in-package :clw-tetris.game.entity)

;; --- parameters --- ;;

(defvar.ps+ *global-params*
    (convert-to-layered-hash
     (:block (:width 20
              :height 20
              :margin 2))))

(defmacro.ps+ get-param (&rest keys)
  `(get-layered-hash *global-params* ,@keys))

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
        (let ((piece-entity (make-piece-entity field)))
          (if piece-entity
              (add-ecs-entity-to-buffer piece-entity)
              (set-entity-param field :gameover-p t)))
        (set-entity-param field :gameover-p t))))

(defun.ps+ move-piece-by-input (entity piece field)
  (labels ((require-move-p (key-name)
             (is-key-down-now key-name))
           (move-if-required (key-name move-direction)
             (when (require-move-p key-name)
               (move-piece-to field piece move-direction))))
    (move-if-required :left :left)
    (move-if-required :right :right)
    (when (require-move-p :down)
      (down-piece-entity entity :piece piece :field field))))

(defun.ps+ rotate-piece-by-input (entity piece field)
  (declare (ignore entity))
  (when (is-key-down-now :a)
    (rotate-piece field piece -1))
  (when (is-key-down-now :c)
    (rotate-piece field piece 1)))

;; TODO: Implement continuous move when continuing to press key
(defun.ps+ process-tetris-input (entity)
  (with-ecs-components (piece) entity
    (let ((field (get-entity-param entity :field)))
      (move-piece-by-input entity piece field)
      (rotate-piece-by-input entity piece field))))

(defun.ps+ fall-in-natural (entity)
  (when (<= (set-entity-param entity :rest-intv
                              (1- (get-entity-param entity :rest-intv)))
            0)
    (down-piece-entity entity)))

;; --- drawing --- ;;

(defun.ps+ calc-block-exist-array-of-next (entity)
  (check-entity-tags entity :next-area)
  (let* ((result (make-array 25 :initial-element nil))
         (field-entity (get-entity-param entity :field-entity))
         (piece-entity (get-entity-param field-entity :next-piece))
         (shape nil))
    (with-ecs-components (piece) piece-entity
      (setf shape (rotate-static-shape (piece-static-shape piece)
                                       (piece-rotate-count piece))))
    (dolist (point shape)
      (let* ((x (car point))
             (y (cadr point))
             (index (+ (+ x 2) (* 5 (+ y 2)))))
        (setf (aref result index) t)))
    result))

(defun.ps+ calc-block-exist-array-of-field (field-entity)
  (check-entity-tags field-entity :field)
  (let* ((field (get-ecs-component 'field field-entity))
         (x-count (field-x-count field))
         (y-count (field-y-count field))
         (result-array (make-array (* x-count y-count)
                                   :initial-element nil)))
    (assert field)
    ;; check static state
    (loop for i from 0 below (* x-count y-count)
       do (when (aref (field-block-state-array field) i)
            (setf (aref result-array i) t)))
    ;; check pieces
    (do-ecs-entities entity
      (when (has-entity-tag entity :piece)
        (with-ecs-components (piece) entity
          (let ((shape (calc-global-piece-shape piece)))
            (dolist (point shape)
              (let ((index (+ (car point)
                              (* (cadr point) (field-x-count field)))))
                (When (< index (* (field-x-count field)
                                  (field-y-count field)))
                  (setf (aref result-array index) t))))))))
    result-array))

(defun.ps+ update-field-draw (entity fn-calc-block-exist-array)
  (check-entity-tags entity :piece-display)
  (let ((block-model-array (get-entity-param entity :block-model-array))
        (block-exist-array (funcall fn-calc-block-exist-array entity))
        (field (get-ecs-component 'field entity)))
    (assert field)
    (assert (= (length block-exist-array) (length block-model-array)))
    (dotimes (i (length block-exist-array))
      (if (aref block-exist-array i)
          (enable-model-2d entity
                           :target-model-2d (aref block-model-array i))
          (disable-model-2d entity
                            :target-model-2d (aref block-model-array i))))))

;; --- entities --- ;;

;; - piece -
(defun.ps+ make-piece-entity (field)
  (let ((entity (make-ecs-entity)))
    (add-entity-tag entity :piece)
    (add-ecs-component-list
     entity
     (init-piece field)
     (init-entity-params :field field
                         :rest-intv 60
                         :intv 60))
    entity))

;; - piece display (next pece area & field) -
(defun.ps+ make-block-model (x y)
  (let ((block-width (get-param :block :width))
        (block-height (get-param :block :height))
        (block-margin (get-param :block :margin)))
    (make-model-2d
     :model (make-solid-rect :width (- block-width (* block-margin 2))
                             :height (- block-height (* block-margin 2))
                             :color #xeeeeee)
     :offset (make-point-2d :x (+ x block-margin)
                            :y (+ y block-margin))
     :depth 0)))

(defun.ps+ make-piece-display-entity (&key x-offset y-offset x-count y-count
                                           fn-script)
  (let ((field (init-tetris-field x-count y-count))
        (block-model-array (make-array (* x-count y-count)))
        (entity (make-ecs-entity))
        (block-width (get-param :block :width))
        (block-height (get-param :block :height)))
    (add-entity-tag entity :piece-display)
    (dotimes (i (* x-count y-count))
      (let* ((x (mod i x-count))
             (y (floor (/ i x-count)))
             (model (make-block-model (* x block-width) (* y block-height))))
        (setf (aref block-model-array i) model)
        (add-ecs-component model entity)))
    (add-ecs-component-list
     entity
     field
     (make-point-2d :x x-offset :y y-offset)
     (init-entity-params :block-model-array block-model-array
                         :gameover-p nil
                         :next-piece (make-piece-entity field))
     (make-script-2d :func fn-script)
     (make-model-2d :model (make-solid-rect :width (* x-count block-width)
                                            :height (* y-count block-height)
                                            :color #x222222)
                    :depth -10))
    entity))

;; - next piece area -
(defun.ps+ update-next-piece-area (entity)
  (check-entity-tags entity :next-area)
  (update-field-draw entity #'calc-block-exist-array-of-next))

(defun.ps+ make-next-piece-area-entity (x y field-entity)
  (check-entity-tags field-entity :field)
  ;; We assume that 5x5 area can cover any pieces,
  ;; if (0,0) of the piece is at the center of the area.
  (let ((entity (make-piece-display-entity
                 :x-count 5 :y-count 5
                 :x-offset x :y-offset y
                 :fn-script #'update-next-piece-area)))
    (add-entity-tag entity :next-area)
    (set-entity-param entity :field-entity field-entity)
    entity))

;; - field -
(defun.ps+ update-field (field-entity)
  (check-entity-tags field-entity :field)
  (do-ecs-entities entity
    (when (has-entity-tag entity :piece)
      (process-tetris-input entity)
      (fall-in-natural entity)))
  (update-field-draw field-entity #'calc-block-exist-array-of-field))

(defun.ps+ make-field-entity (&key x-count y-count)
  (let ((entity (make-piece-display-entity
                 :x-count x-count :y-count y-count
                 ;; TODO: Parameterize
                 :x-offset 10 :y-offset 10
                 :fn-script #'update-field)))
    (add-entity-tag entity :field)
    entity))

;; - initalize all entities -
(defun.ps+ init-tetris-entities ()
  (let* ((field-entity (make-field-entity
                        :x-count 10
                        :y-count 15))
         (field (get-ecs-component 'field field-entity))
         (next-piece-area (make-next-piece-area-entity 300 300 field-entity)))
    (assert field)
    (add-ecs-entity field-entity)
    (add-ecs-entity (make-piece-entity field))
    (add-ecs-entity next-piece-area field-entity)))
