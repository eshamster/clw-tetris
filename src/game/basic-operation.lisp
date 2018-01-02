(in-package :cl-user)
(defpackage clw-tetris.game.basic-operation
  (:use :cl
        :ps-experiment
        :cl-ps-ecs
        :cl-web-2d-game)
  (:export :rotate-piece

           :field
           :init-tetris-field
           :field-x-count
           :field-y-count
           :field-block-state-array

           :piece
           :init-piece
           :clone-piece
           :copy-piece-to
           :piece-static-shape
           :piece-x
           :piece-y
           :piece-rotate-count

           :rotate-static-shape

           :calc-global-piece-shape
           :enable-to-move-piece-to-p
           :move-piece-to
           :rotate-piece

           :intersect-piece-to-field-p
           :pin-piece-to-field))
(in-package :clw-tetris.game.basic-operation)

;; TODO: Consider mirrored shapes

;; Note: y = 0 is bottom of a field.
;;       x = 0 is left of a field.

;; --- structures and basic functions --- ;;

(defstruct.ps+ (field (:include ecs-component))
    x-count y-count block-state-array)

(defstruct.ps+ (piece (:include ecs-component))
    static-shape x y rotate-count rotatable)

;; Because static-shape never be modified, shallow-copying is enough.
(defun.ps+ copy-piece-to (dst src)
  (macrolet ((set-param (param)
               `(setf (,param dst) (,param src))))
    (set-param piece-static-shape)
    (set-param piece-x)
    (set-param piece-y)
    (set-param piece-rotate-count)))

(defun.ps+ clone-piece (piece)
  (with-slots (static-shape x y rotate-count) piece
    (make-piece :static-shape static-shape
                :x x :y y :rotate-count rotate-count)))

;; --- field functions --- ;;

(defun.ps+ init-tetris-field (x-count y-count)
  (make-field :x-count x-count
              :y-count y-count
              :block-state-array (make-array (* x-count y-count)
                                             :initial-element nil)))

(defun.ps+ check-in-field-p (field x y)
  (with-slots (x-count y-count) field
    (assert (and (>= x 0) (< x x-count)
                 (>= y 0) (< y y-count)))))

(defmacro.ps+ get-block-state% (field x y)
  `(aref (field-block-state-array ,field)
         (+ ,x (* ,y (field-x-count ,field)))))

(defun.ps+ get-block-state (field x y)
  (check-in-field-p field x y)
  (get-block-state% field x y))

(defun.ps+ set-block-state (field x y state)
  (check-in-field-p field x y)
  (setf (get-block-state% field x y) state))

(defun.ps+ delete-completed-lines (field)
  (with-slots (x-count y-count block-state-array) field
    (let ((completed-index-list
           (loop for y
              from 0 below y-count
              when (= (length (loop for x from 0 below x-count
                                 when (not (get-block-state field x y))
                                 collect t))
                      0)
              collect y)))
      ;; naive implementation...
      (let ((delete-count 0))
        (dolist (i completed-index-list)
          (loop for y from (- i delete-count) below (1- y-count)
             do (loop for x from 0 below x-count
                   do (set-block-state field x y (get-block-state field x (1+ y)))))
          (loop for x from 0 below x-count
             do (set-block-state field x (1- y-count) nil))
          (incf delete-count))
        delete-count))))

;; --- piece functions --- ;;

(defstruct.ps+ static-shape-param point-list (rotatable t))

;; Note: (0 0) is a center of rotation
(defvar.ps+ *static-shape-list*
    (list
     ;; ----
     (make-static-shape-param :point-list '((-1 0) (0 0) (1 0) (2 0)))
     ;; _|^
     (make-static-shape-param :point-list '((-1 0) (0 0) (0 1) (1 1)))
     ;; |-
     (make-static-shape-param :point-list '((0 -1) (0 0) (1 0) (0 1)))
     ;; __|
     (make-static-shape-param :point-list '((-2 0) (-1 0) (0 0) (0 1)))
     ;; |__
     (make-static-shape-param :point-list '((-1 1) (-1 0) (0 0) (1 0)))
     ;; =
     (make-static-shape-param :point-list '((0 0) (0 1) (1 0) (1 1))
                              :rotatable nil)))

(defun.ps+ init-piece (field)
  (let* ((static-shape (nth (random (length *static-shape-list*))
                            *static-shape-list*))
         (rotatable (static-shape-param-rotatable static-shape))
         (piece (make-piece :x (/ (field-x-count field) 2)
                            :y 0 ; temporal value
                            :static-shape (static-shape-param-point-list
                                           static-shape)
                            :rotatable rotatable
                            :rotate-count (if rotatable (random 4) 0)))
         (global-shape (calc-global-piece-shape piece))
         (min-y (cadr (aref global-shape 0))))
    (loop for i from 1 below (length global-shape)
       do (let ((y (cadr (aref global-shape i))))
            (when (< y min-y)
              (setf min-y y))))
    (setf (piece-y piece)
          (- (field-y-count field) min-y 1))
    piece))

(defun.ps+ rotate-static-shape (shape rotate-count)
  (mapcar (lambda (point)
            (let ((x (car point))
                  (y (cadr point)))
              (case (mod rotate-count 4)
                (0 (list x y))
                (1 (list y (* -1 x)))
                (2 (list (* -1 x) (* -1 y)))
                (3 (list (* -1 y) x))
                (t (error "Invalid rotate-count: ~D" rotate-count)))))
          shape))

(defun.ps+ calc-global-piece-shape (piece)
  (with-slots (static-shape x y rotate-count) piece
    (let ((result (rotate-static-shape static-shape rotate-count)))
      (dolist (pnt result)
        (incf (car pnt) x)
        (incf (cadr pnt) y))
      result)))

(defun.ps+ enable-to-move-piece-to-p (field piece direction)
  (let ((shape (calc-global-piece-shape piece)))
    (dolist (point shape)
      (ecase direction
        (:down (decf (cadr point)))
        (:right (incf (car point)))
        (:left (decf (car point)))
        (:there ; do nothing
         )))
    (every (lambda (point)
             (let ((x (car point))
                   (y (cadr point)))
               (and (>= x 0) (< x (field-x-count field))
                    ;; Note: Can exceed top when moving
                    (>= y 0)
                    (or (>= y (field-y-count field))
                        (not (get-block-state field x y))))))
           shape)))

(defun.ps+ move-piece-to (field piece direction)
  "Move the piece to the direction if possible.
Return t if the piece was moved, otherwize nil"
  (when (enable-to-move-piece-to-p field piece direction)
    (with-slots (x y) piece
      (ecase direction
        (:down (decf y))
        (:right (incf x))
        (:left (decf x))
        (:there ; do nothing
         )))
    t))

(defun.ps+ intersect-piece-to-field-p (field piece)
  (some (lambda (point)
          (let ((x (car point))
                (y (cadr point)))
            (when (< y (field-y-count field))
              (get-block-state field x y))))
        (calc-global-piece-shape piece)))

(defun.ps+ rotate-piece (field piece added-rotate-count)
  (unless (piece-rotatable piece)
    (return-from rotate-piece t))
  (let ((cloned-piece (clone-piece piece)))
    (with-slots (rotate-count x) cloned-piece
      (setf rotate-count (mod (+ rotate-count added-rotate-count) 4))
      (labels ((adjust-x (new-x loop-count)
                 (assert (< loop-count (field-x-count field)))
                 (setf x new-x)
                 (let ((shape (calc-global-piece-shape cloned-piece)))
                   (when (some (lambda (point) (< (car point) 0))
                               shape)
                     (adjust-x (1+ new-x) (1+ loop-count)))
                   (when (some (lambda (point) (>= (car point) (field-x-count field)))
                               shape)
                     (adjust-x (1- new-x) (1+ loop-count))))))
        (adjust-x x 0))
      (unless (intersect-piece-to-field-p field cloned-piece)
        (setf (piece-rotate-count piece) rotate-count
              (piece-x piece) x)
        t))))

(defun.ps+ pin-piece-to-field (field piece)
  "Pin the piece to the field. After pinning, delete completed lines.
Return nil if game over situation."
  (with-slots (y-count) field
    (labels ((pin-block-to-field (field x y)
               (when (< y y-count)
                 (assert (not (get-block-state field x y)))
                 (set-block-state field x y t)
                 t))
             (pin-all-blocks-to-field (field blocks &optional (y-offset 0))
               (let ((pending-blocks '()))
                 (dolist (point blocks)
                   (let ((x (car point))
                         (y (- (cadr point) y-offset)))
                     (unless (pin-block-to-field field x y)
                       (push point pending-blocks))))
                 pending-blocks)))
      (let* ((pending-blocks
              (pin-all-blocks-to-field field (calc-global-piece-shape piece)))
             (count-deleted (delete-completed-lines field))
             (rest-blocks
              (pin-all-blocks-to-field field pending-blocks count-deleted)))
        (= (length rest-blocks) 0)))))
