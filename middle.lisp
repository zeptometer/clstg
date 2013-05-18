(shooting.utils:namespace shooting.middle
  (:use :common-lisp
	:iterate
	:shooting.core)
  (:export :<circle-territory>
	   :.radius
	   :<directed>
	   :.speed
	   :.direction
	   :<dxdy>
	   :.dx
	   :.dy
	   :<circle-shape>
	   :<fixed>
	   :<out-to-die>
	   :fix-position
	   :<full-search>))

;; territory
(defclass <circle-territory> (<territory>)
  ((radius :initarg :radius
	   :accessor .radius)))

(defmethod collidep ((obj1 <object>) (obj2 <object>) 
		     (trr1 <circle-territory>) (trr2 <circle-territory>))
  (labels ((sqr (x) (* x x)))
    (<= (+ (sqr (- (.x obj1) (.x obj2))) (sqr (- (.y obj1) (.y obj2)))) 
	(sqr (+ (.radius trr1) (.radius trr2))))))

;; object
(defclass <fixed> (<object>) ())
(defgeneric fix-position (obj))

(defmethod update :after ((obj <fixed>))
  (fix-position obj))


(defclass <out-to-die> (<object>) ())
(defgeneric out-of-world-p (obj))

(defmethod update :after ((obj <out-to-die>))
  (when (out-of-world-p obj)
    (discard obj)))


(defclass <directed> (<object>)
  ((speed :type float
	  :accessor .speed
	  :initarg :speed)
   (direction :type float
	      :accessor .direction
	      :initarg :direction
	      :initform 0.0)))

(defmethod update :after ((obj <directed>))
  (with-accessors ((x .x) (y .y) (speed .speed) (direction .direction)) obj
    (incf x (* speed (cos direction)))
    (incf y (* speed (sin direction)))))



(defclass <dxdy> (<object>)
  ((dx :type float
       :accessor .dx
       :initarg :dx
       :initform 0.0)
   (dy :type float
       :accessor .dy
       :initarg :dy
       :initform 0.0)))

(defmethod update :after ((obj <dxdy>))
  (with-accessors ((x .x) (y .y) (dx .dx) (dy .dy)) obj
    (incf x dx)
    (incf y dy)))



(defclass <circle-shape> (<object>)
  ((shooting.core::territory :type <circle-territory>
	      :initarg :territory
	      :accessor .territory)
   (color :type sdl:color
	  :initarg :color
	  :accessor .color)))

(defmethod .radius ((obj <circle-shape>))
  (.radius (.territory obj)))

(defmethod initialize-instance :after ((instance <circle-shape>) &key radius &allow-other-keys)
  (setf (.territory instance)
	(make-instance '<circle-territory> :radius radius)))

(defmethod fix-position ((obj <circle-shape>))
  (with-accessors ((x .x) (y .y) (r .radius)) obj
    (when (< x r) (setf x r))
    (when (> x (- *screen-width* r)) (setf x (- *screen-width* r)))
    (when (< y r) (setf y r))
    (when (> y (- *screen-height* r)) (setf y (- *screen-height* r)))))

(defmethod out-of-world-p ((obj <circle-shape>))
  (with-accessors ((x .x) (y .y) (r .radius)) obj
    (or (< x (- r)) 
	(> x (+ *screen-width* r))
	(< y (- r))
	(> y (+ *screen-height* r)))))

(defmethod draw ((obj <circle-shape>))
  (sdl:draw-filled-circle-* (floor (.x obj)) (floor (.y obj)) (floor (.radius (.territory obj))) :color (.color obj)))


;; world
(defclass <full-search> (<world>)
  ((registered-objects :initform nil
		      :accessor .registered-objects)
   (tobe-registered-objects :initform nil
			    :accessor .tobe-registered-objects))
  (:documentation  "detect collidion with full-search. simple but inefficient"))

(defmethod register-object (obj (world <full-search>))
  (push obj (.tobe-registered-objects world)))

(defmethod update-world ((world <full-search>))
  (with-accessors ((registered .registered-objects)
		   (tobe-registered .tobe-registered-objects)) world
    ;; update objects
    (iter (for obj in registered)
	  (update obj))

    ;; detect collision
    (iter (for obj1 in registered)
	  (for rest on (rest registered))
	  (iter (for obj2 in rest)
		(when (and (collidablep obj1 obj2)
			   (collidep obj1 obj2 (.territory obj1) (.territory obj2)))
		  (collide obj1 obj2)
		  (collide obj2 obj1))))

    ;; remove dead objects
    (setf registered (nconc tobe-registered (remove-if-not #'.alivep registered))
	  tobe-registered nil)))

(defmethod draw-world ((world <full-search>))
  (iter (for obj in (.registered-objects world))
	(draw obj)))
