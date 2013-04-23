(in-package :shooting.core)

(defkeystate key-state
  (up    :sdl-key-w)
  (down  :sdl-key-s)
  (right :sdl-key-d)
  (left  :sdl-key-a)
  (space :sdl-key-space))

;;; player
(defclass <player> (<object>)
  ((charge :accessor .charge
	    :initform 0)))

(defmethod create ((name (eql '<player>)) &key x y speed img &allow-other-keys)
  (push (make-instance '<player> :x x :y y :speed speed :img img :territory (make-instance '<circle-territory> :radius 25)) *tobe-registered-objects*))

(defmethod update ((obj <player>) world)
  (with-slots (up down right left space) world
    (with-accessors ((x .x) (y .y) (speed .speed) (charge .charge)) obj
      (let ((dx 0.0)
	    (dy 0.0))
	(when up    (decf dy speed))
	(when down  (incf dy speed))
	(when left  (decf dx speed))
	(when right (incf dx speed))
	(when (and (/= dx 0.0) (/= dy 0.0))
	  (setf dx (/ dx (sqrt 2.0))
		dy (/ dy (sqrt 2.0))))
	(incf x dx)
	(incf y dy)
	(fix-position obj)

	(when (and space (<= charge 0))
	  (create '<bullet> :x x :y y)
	  (incf charge 5))
	(when (> charge 0)
	  (decf charge))))))

(defmethod draw ((obj <player>))
  (sdl:draw-surface-at-* (.img obj) (- (round (.x obj)) 50) (- (round (.y obj)) 50)))

(defmethod fix-position ((obj <player>))
  (with-accessors ((x .x) (y .y)) obj
    (when (< x 50) (setq x 50))
    (when (> x (- *screen-width* 50))  (setq x (- *screen-width* 50)))
    (when (< y 50) (setq y 50))
    (when (> y (- *screen-height* 50)) (setq y (- *screen-height* 50)))))

;;; bullet
(defclass <bullet> (<object>)
  ())

(defmethod create ((name (eql '<bullet>)) &key x y &allow-other-keys)
  (push (make-instance '<bullet> :x x :y (- y 50) :speed 10 :territory (make-instance '<circle-territory> :radius 5)) *tobe-registered-objects*))

(defmethod update ((obj <bullet>) world)
  (with-accessors ((y .y) (speed .speed)) obj
    (decf y speed))
  (when (< (.y obj) 5)
    (setf (.alivep obj) nil)))

(defmethod collide ((collidee <bullet>) collider)
  (declare (ignore collidee collider)))

(defmethod draw ((obj <bullet>))
  (with-accessors ((x .x) (y .y)) obj
    (sdl:draw-filled-circle-* (round x) (round y) 5 :color sdl:*cyan*)))

;;; enemy1
(defclass <enemy1> (<object>)
  ((direction :initform (random (* 2 +pi+))
	      :accessor .direction)))

(defmethod create ((name (eql '<enemy1>)) &key x y speed &allow-other-keys)
  (push (make-instance '<enemy1> :x x :y y :speed speed :img "hoge.png" :territory (make-instance '<circle-territory> :radius 25)) *tobe-registered-objects*))

(defmethod update ((obj <enemy1>) world)
  (with-accessors ((x .x) (y .y) (speed .speed) (direction .direction)) obj
    (incf x (* speed (cos direction)))
    (decf y (* speed (sin direction)))

    (fix-position obj)))

(defmethod fix-position ((obj <enemy1>))
  (with-accessors ((x .x) (y .y) (speed .speed) (direction .direction)) obj
    (cond ((< x 25)
	   (setf direction (- +pi+ direction)
		 x 25))
	  ((> x (- *screen-width* 25))
	   (setf direction (- +pi+ direction)
		 x (- *screen-width* 25)))
	  ((< y 25)
	   (setf direction (- direction)
		 y 25))
	  ((> y (- *screen-height* 25))
	   (setf direction (- direction)
		 y (- *screen-height* 25))))))

(defmethod collide ((collidee <enemy1>) collider)
  (setf (.alivep collidee) nil))

(defmethod collidablep ((obj1 <enemy1>) (obj2 <bullet>)) (declare (ignore obj1 obj2)) t)
(defmethod collidablep ((obj1 <bullet>) (obj2 <enemy1>)) (declare (ignore obj1 obj2)) t)

(defmethod draw ((obj <enemy1>))
  (with-accessors ((x .x) (y .y) (img .img)) obj
    (sdl:draw-surface-at-* img (- (round x) 25) (- (round y) 25))))
