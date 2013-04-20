(shooting.utils:namespace shooting.core
  (:use :common-lisp
	:iterate))

;;;variables
(defconstant +pi+ 3.141592653)

(defparameter *screen-width* 640)
(defparameter *screen-height* 480)

(defparameter *tobe-registered-objects* nil)
(defparameter *registered-objects* nil)

;;; utility
(defun load-png-image (source-file)
  (sdl:convert-to-display-format :surface (sdl:load-image source-file)
				 :enable-alpha t
				 :pixel-alpha t))

;;; keyboard input
(defgeneric update-key-state (key key-press key-state ))

(defmacro defkeystate (name &rest key-maps)
  `(progn
     (defclass ,name ()
       ,(loop :for k in key-maps collect `(,(car k) :initform nil)))
     ,(shooting.utils:with-gensyms (key key-press key-state)
        `(defmethod update-key-state (,key ,key-press (,key-state ,name))
	   (with-slots ,(mapcar #'car key-maps) ,key-state
	     (cond ,@(loop :for k :in key-maps
			:collect `((sdl:key= ,key ,(cadr k))
				   (setf ,(car k) ,key-press)))))))))

(defkeystate key-state
  (up    :sdl-key-w)
  (down  :sdl-key-s)
  (right :sdl-key-d)
  (left  :sdl-key-a)
  (space :sdl-key-space))

;;; territory
(defclass <territory> ()
 ())

(defclass <circle-territory> (<territory>)
  ((radius :initarg :radius
	   :accessor .radius)))

;;; object
(defclass <object> ()
  ((x :initarg :x
      :accessor .x)
   (y :initarg :y
      :accessor .y)
   (speed :initarg :speed
	  :accessor .speed)
   (alivep :initform t
	   :accessor .alivep)
   (territory :type <territory>
	      :initarg :territory
	      :accessor .territory)
   (img :accessor .img)))

(defmethod initialize-instance :after ((instance <object>) &key ((:img source) nil) &allow-other-keys)
  (when source
    (setf (.img instance) (load-png-image source))))

(defgeneric create (name &key &allow-other-keys))
(defgeneric update (obj world))
(defgeneric draw (obj))
(defgeneric collidablep (obj1 obj2))
(defmethod collidablep ((obj1 <object>) (obj2 <object>))
  nil)
(defgeneric collidep (obj1 obj2 trr1 trr2))
(defmethod collidep (obj1 obj2 (trr1 <territory>) (trr2 <territory>))
  nil)
(defgeneric collide (collidee collider))

(defgeneric fix-position (obj))


(defun sqr (x) (* x x))
(defmethod collidep ((obj1 <object>) (obj2 <object>) (trr1 <circle-territory>) (trr2 <circle-territory>))
   (<= (+ (sqr (- (.x obj1) (.x obj2))) (sqr (- (.y obj1) (.y obj2)))) (sqr (+ (.radius trr1) (.radius trr2)))))

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


;;; main loop
(defun main ()
  (setf *registered-objects* nil)
  (sdl:with-init ()
    (sdl:window *screen-width* *screen-height* :title-caption "test") 
    (setf (sdl:frame-rate) 60) 
    (sdl:initialise-default-font sdl:*font-10x20*)

    (let ((current-key-state (make-instance 'key-state)))
      (create '<player> :x 0 :y 0 :speed 5 :img "test.png")
      (create '<enemy1> :x 120 :y 120 :speed 3)
      (sdl:update-display)

      (sdl:with-events ()
	(:quit-event () t)
	(:key-down-event (:key key)
	  (if (sdl:key= key :sdl-key-escape)
	      (sdl:push-quit-event)
	      (update-key-state key t current-key-state)))
	(:key-up-event (:key key)
	  (update-key-state key nil current-key-state))
	(:idle ()
	  (sdl:clear-display sdl:*black*)

	  ;; update each object
	  (iter (for obj in *registered-objects*)
		(update obj current-key-state))

	  ;; detect collidion
	  ;; This collidion-detect uses full search, and is unefficient when (length *registered-objects*) is large.
	  ;; More efficient algorithm such as quadtree should be used.
	  (iter (for obj1 in *registered-objects*)
		(iter (for obj2 in *registered-objects*)
		      (when (and (not (eq obj1 obj2))
				 (collidablep obj1 obj2)
				 (collidep obj1 obj2 (.territory obj1) (.territory obj2)))
			(collide obj1 obj2))))

	  ;; update *registered-objects*
	  (let ((alive (iter (for obj in *registered-objects*)
			     (when (.alivep obj) (collect obj)))))
	    (setf *registered-objects*
		  (nconc alive *tobe-registered-objects*)
		  *tobe-registered-objects* nil))
	  
	  ;; draw alive objects
	  (iter (for obj in *registered-objects*)
		(draw obj))
	  (sdl:update-display))))))
