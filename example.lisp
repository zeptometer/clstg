(shooting.utils:namespace example-game
  (:use :common-lisp
	:iterate
	:shooting.core)
  (:export :*example-game*))

(in-package example-game)

(defkeystate key-state
  (up    :sdl-key-w)
  (down  :sdl-key-s)
  (right :sdl-key-d)
  (left  :sdl-key-a)
  (space :sdl-key-space))


;;; game
(defclass example (<game>) 
  ((player-size :initarg :player-size
		:initform 20)
   (player-speed :initarg :player-speed
		 :initform 5)))

(defmethod initialize-game ((game example))
  (with-slots (height width schedule player-size player-speed) game
    (setf *screen-height* height
	  *screen-width* width)
    (create 'player
	    :x (/ width 2)
	    :y (- height 10 (/ player-size 2))
	    :speed player-speed
	    :radius (/ player-size 2))
    (make-instance 'example-world
		   :schedule schedule
		   :key-state (make-instance 'key-state))))

;;; world
(defclass example-world (<world>) ())

(defmethod update-world ((world example-world))
  nil)

(defmethod draw-world ((world example-world))
  nil)

;;; player
(defclass player (<object>)
  ((speed :initarg :speed
	  :accessor .speed)
   (radius :initarg :radius
	   :accessor .radius)
   (charge :initform 20
	   :accessor .charge)))

(defmethod create ((name (eql 'player)) &key x y speed radius)
  (register-object
   (make-instance 'player 
		  :x x
		  :y y
		  :speed speed
 		  :radius radius
		  :territory (make-instance '<circle-territory> :radius radius))))

(defun fix-position (player)
  (with-accessors ((x .x) (y .y) (r .radius)) player
    (when (< x r) (setf x r))
    (when (> x (- *screen-width* r)) (setf x (- *screen-width* r)))
    (when (< y r) (setf y r))
    (when (> y (- *screen-height* r)) (setf y (- *screen-height* r)))))

(defmethod update ((obj player) (world <world>))
  (with-slots (up down right left space) (.key-state world)
    (with-accessors ((x .x) (y .y) (r .radius) (speed .speed) (charge .charge)) obj
      (let ((dx 0)
	    (dy 0))
	(when up (decf dy speed))
	(when down (incf dy speed))
	(when left (decf dx speed))
	(when right (incf dx speed))
	(unless (or (zerop dx) (zerop dy))
	  (setf dx (/ dx 1.41421356)
		dy (/ dy 1.41421356)))
	(incf x dx)
	(incf y dy)
	(fix-position obj))

      (when (< charge 20)
	(incf charge))
      (when (and space (= charge 20))
	(create 'bullet :x x :y (- y r 5) :radius 5 :speed 10)
	(setf charge 0)))))

(defmethod draw ((obj player))
  (with-accessors ((x .x) (y .y) (r .radius)) obj
    (sdl:draw-filled-circle-* (floor x) (floor y) (floor r) :color sdl:*blue*)))


;;; bullet
(defclass bullet (<object>)
  ((speed :initarg :speed
	  :accessor .speed)
   (radius :initarg :radius
	   :accessor .radius)))

(defmethod create ((name (eql 'bullet)) &key x y speed radius)
  (register-object 
   (make-instance 'bullet
		  :x x
		  :y y
		  :speed speed
		  :radius radius
		  :territory (make-instance '<circle-territory> :radius radius))))

(defmethod update ((obj bullet) (world example-world))
  (with-accessors ((y .y) (speed .speed) (r .radius)) obj
    (decf y speed)
    (when (< y r) (discard obj))))

(defmethod draw ((obj bullet))
  (with-accessors ((x .x) (y .y) (r .radius)) obj
    (sdl:draw-filled-circle-* (floor x) (floor y) (floor r) :color sdl:*white*)))


(defparameter *example-game*
  (make-instance 'example))

;; (with-accessors ((x .x) (y .y) (r .radius))  (car shooting.core::*registered-objects*)
;;   (list x y r))
