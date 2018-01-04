(in-package :cl-user)
(defpackage clw-tetris.game.entity
  (:use :cl
        :ps-experiment
        :cl-ps-ecs
        :cl-web-2d-game
        :clw-tetris.game.basic-operation)
  (:export :init-tetris-entities
           :process-with-field-and-piece
           :warp-piece-to
           :gameover-p))
(in-package :clw-tetris.game.entity)

;; --- parameters --- ;;

(defvar.ps+ *global-params*
    (convert-to-layered-hash
     (:block (:width 30
              :height 30
              :margin 2)
      :field (:x-count 10
              :y-count 15)
      :next-piece-area (:dist-from-field 30)
      :input (:first-intv 15
              :intv 4))))

(defmacro.ps+ get-param (&rest keys)
  `(get-layered-hash *global-params* ,@keys))

;; --- gaming --- ;;

(defun.ps+ change-field-entity-to-gameover (field-entity)
  (set-entity-param field-entity :gameover-p t))

(defun.ps+ gameover-p (field-entity)
  (get-entity-param field-entity :gameover-p))

;; --- moving --- ;;

(defun.ps+ process-with-field-and-piece (field-entity fn-process)
  (let ((piece-entity (get-entity-param field-entity :current-piece)))
    (with-ecs-components (field) field-entity
      (with-ecs-components (piece) piece-entity
        (funcall fn-process field piece)))))

(defun.ps+ reset-piece-down-interval (piece-entity)
  (set-entity-param piece-entity :rest-intv
                    (get-entity-param piece-entity :intv)))

(defun.ps+ warp-piece-to (field-entity new-piece)
  (when (gameover-p field-entity)
    (return-from warp-piece-to))
  (check-type new-piece piece)
  (process-with-field-and-piece
   field-entity
   (lambda (field current-piece)
     (let ((current-y (piece-y current-piece))
           (new-y (piece-y new-piece)))
       (when (> new-y current-y)
         (error "A piece can't move to up (before: ~A, after: ~A)" current-y new-y))
       (when (move-piece-to field new-piece :there)
         (when (< new-y current-y)
           (reset-piece-down-interval (get-entity-param field-entity :current-piece)))
         (copy-piece-to current-piece new-piece))))))

(defun.ps+ swap-piece-to-next (field-entity field)
  (let ((next-piece-entity (get-entity-param field-entity :next-piece)))
    (add-ecs-entity-to-buffer next-piece-entity)
    (set-entity-param field-entity :current-piece next-piece-entity)
    (set-entity-param field-entity :next-piece
                      (make-piece-entity field))
    (with-ecs-components ((next-piece piece)) next-piece-entity
      (when (intersect-piece-to-field-p field next-piece)
        (change-field-entity-to-gameover field-entity)))))

(defun.ps+ down-piece-entity (field-entity)
  (check-entity-tags field-entity :field)
  (let ((piece-entity (get-entity-param field-entity :current-piece)))
    (reset-piece-down-interval piece-entity)
    (process-with-field-and-piece
     field-entity
     (lambda (field current-piece)
       (unless (move-piece-to field current-piece :down)
         (register-next-frame-func (lambda ()
                                     (when (find-the-entity piece-entity)
                                       (delete-ecs-entity piece-entity))))
         (if (pin-piece-to-field field current-piece)
             (swap-piece-to-next field-entity field)
             (change-field-entity-to-gameover field-entity)))))))

(defun.ps+ move-piece-by-input (field-entity)
  (labels ((require-move-p (key-name)
             (let ((count (key-down-count key-name))
                   (first-intv (get-param :input :first-intv)))
               (or (= count 1)
                   (and (> count (1+ first-intv))
                        (= 0 (mod (- count 1 first-intv)
                                  (get-param :input :intv)))))))
           (move-if-required (key-name move-direction)
             (when (require-move-p key-name)
               (process-with-field-and-piece
                field-entity
                (lambda (field piece)
                  (move-piece-to field piece move-direction))))))
    (move-if-required :left :left)
    (move-if-required :right :right)
    (when (require-move-p :down)
      (down-piece-entity field-entity))))

(defun.ps+ rotate-piece-by-input (field-entity)
  (process-with-field-and-piece
   field-entity
   (lambda (field piece)
     (when (is-key-down-now :a)
       (rotate-piece field piece -1))
     (when (is-key-down-now :c)
       (rotate-piece field piece 1)))))

(defun.ps+ process-tetris-input (field-entity)
  (move-piece-by-input field-entity)
  (rotate-piece-by-input field-entity))

(defun.ps+ fall-in-natural (field-entity)
  (let ((piece-entity (get-entity-param field-entity :current-piece)))
    (when (<= (set-entity-param piece-entity :rest-intv
                                (1- (get-entity-param piece-entity :rest-intv)))
              0)
      (down-piece-entity field-entity))))

;; --- drawing --- ;;

;; We assume that 5x5 area can cover any pieces,
;; if (0,0) of the piece is at the center of the area.
(defvar.ps+ *next-piece-area-length* 5)

(defun.ps+ calc-block-exist-array-of-next (entity)
  (check-entity-tags entity :next-area)
  (let* ((result (make-array (expt *next-piece-area-length* 2)
                             :initial-element nil))
         (field-entity (get-entity-param entity :field-entity))
         (piece-entity (get-entity-param field-entity :next-piece))
         (shape nil))
    (with-ecs-components (piece) piece-entity
      (setf shape (rotate-static-shape (piece-static-shape piece)
                                       (piece-rotate-count piece))))
    (dolist (point shape)
      (let* ((x (car point))
             (y (cadr point))
             (index (+ (+ x 2) (* *next-piece-area-length* (+ y 2)))))
        (setf (aref result index) t)))
    result))

(defun.ps+ calc-block-exist-array-of-field (field-entity)
  (check-entity-tags field-entity :field)
  (process-with-field-and-piece
   field-entity
   (lambda (field piece)
     (let* ((x-count (field-x-count field))
            (y-count (field-y-count field))
            (result-array (make-array (* x-count y-count)
                                      :initial-element nil)))
       (assert field)
       ;; check static state
       (loop for i from 0 below (* x-count y-count)
          do (when (aref (field-block-state-array field) i)
               (setf (aref result-array i) t)))
       ;; check pieces
       (let ((shape (calc-global-piece-shape piece)))
         (dolist (point shape)
           (let ((index (+ (car point)
                           (* (cadr point) (field-x-count field)))))
             (When (< index (* (field-x-count field)
                               (field-y-count field)))
               (setf (aref result-array index) t)))))
       result-array))))

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
                         :x-count x-count
                         :y-count y-count
                         :block-width block-width
                         :block-height block-height)
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
  (let ((entity (make-piece-display-entity
                 :x-count *next-piece-area-length*
                 :y-count *next-piece-area-length*
                 :x-offset x :y-offset y
                 :fn-script #'update-next-piece-area)))
    (add-entity-tag entity :next-area)
    (set-entity-param entity :field-entity field-entity)
    entity))

;; - field -
(defun.ps+ update-field (field-entity)
  (check-entity-tags field-entity :field)
  (unless (gameover-p field-entity)
    (process-tetris-input field-entity)
    (fall-in-natural field-entity))
  (update-field-draw field-entity #'calc-block-exist-array-of-field))

(defun.ps+ make-field-entity (&key x-count y-count)
  (let ((entity (make-piece-display-entity
                 :x-count x-count :y-count y-count
                 :x-offset (/ (- (get-screen-width)
                                 (* x-count (get-param :block :width)))
                              2)
                 :y-offset (/ (- (get-screen-height)
                                 (* y-count (get-param :block :height)))
                              2)
                 :fn-script #'update-field)))
    (add-entity-tag entity :field)
    (with-ecs-components (field) entity
      (set-entity-param entity :current-piece (make-piece-entity field))
      (set-entity-param entity :next-piece (make-piece-entity field))
      (set-entity-param entity :gameover-p nil))
    entity))

;; - initalize all entities -
(defun.ps+ init-tetris-entities ()
  (let* ((x-count (get-param :field :x-count))
         (y-count (get-param :field :y-count))
         (field-entity (make-field-entity
                        :x-count x-count
                        :y-count y-count))
         (field (get-ecs-component 'field field-entity))
         (next-piece-area (make-next-piece-area-entity
                           (+ (* x-count (get-param :block :width))
                              (get-param :next-piece-area :dist-from-field))
                           (* (- y-count (1+ *next-piece-area-length*))
                              (get-param :block :height))
                           field-entity)))
    (assert field)
    (add-ecs-entity field-entity)
    (add-ecs-entity (get-entity-param field-entity :current-piece))
    (add-ecs-entity next-piece-area field-entity)))
