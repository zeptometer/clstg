(shooting.utils:namespace example-game2
			  (:use :common-lisp
				:iterate
				:shooting.core
				:shooting.middle)
			  (:export :*example-game*))

(in-package example-game2)

(defkeystate key-state
    (up    :sdl-key-w)
  (down  :sdl-key-s)
  (right :sdl-key-d)
  (left  :sdl-key-a)
  (space :sdl-key-space))


;;; game
(defclass example (<game>) 
  ((player-size :initarg :player-size)
   (player-speed :initarg :player-speed)))

(defmethod initialize-game ((game example))
  (with-slots (height width schedule player-size player-speed) game
    (setf *screen-height* height
	  *screen-width* width
	  *world* (make-instance '<full-search>
				 :schedule schedule
				 :key-state (make-instance 'key-state)))
    (create player
	    :x (float (/ width 2))
	    :y (float (- height 10 (/ player-size 2)))
	    :speed (float player-speed)
	    :radius (float (/ player-size 2))
	    :charge-limit 5
	    :color sdl:*white*)))


;;; enemy
(defclass enemy (<circle-shape> <fixed> <directed>)
  ((timer-limit :reader .timer-limit
		:initarg :timer-limit)
   (timer :accessor .timer
	  :initform 0)
   (hitpoint :accessor .hitpoint
	     :initarg :hitpoint)))

(defmethod update ((obj enemy))
  (with-accessors ((dir .direction)
		   (timer .timer)
		   (limit .timer-limit)) obj
    (incf dir 0.1)
    (if (= timer 0)
	(progn
	  (create enemy-bullet 
		  :x (.x obj)
		  :y (.y obj)
		  :speed -3.0
		  :acc 0.1
		  :radius 3.0
		  :dspeed 0.01
		  :direction dir
		  :color sdl:*yellow*)
	  (create enemy-bullet 
		  :x (.x obj)
		  :y (.y obj)
		  :speed -3.0
		  :acc 0.05
		  :radius 3.0
		  :dspeed 0.01
		  :direction (+ dir 10)
		  :color sdl:*yellow*)
	  (setf timer limit))
	(decf timer))))

(defmethod collide ((collidee enemy) colliider)
  (with-accessors ((hitpoint .hitpoint)) collidee
      (decf hitpoint)
    (when (zerop hitpoint)
      (discard collidee))))


(defclass enemy-bullet (<circle-shape> <out-to-die> <directed>)
  ((acc :accessor .acc
	:initform 0.0
	:initarg :acc)
   (dspeed :accessor .dspeed
	   :initform 0.0
	   :initarg :dspeed)))

(defmethod update ((obj enemy-bullet))
  (incf (.speed obj) (.acc obj))
  (incf (.direction obj) (.dspeed obj)))

;;; player
(defclass player (<circle-shape> <fixed> <dxdy>)
  ((speed :accessor .speed
	  :initarg :speed)
   (charge-limit :reader .charge-limit
		 :initarg :charge-limit)
   (charge :accessor .charge
	   :initform 0)))

(defmethod update ((obj player))
  (with-slots (up down right left space) (.key-state *world*)
    (with-accessors ((speed .speed)
		     (x .x)
		     (y .y)
		     (dx .dx)
		     (dy .dy)
		     (charge .charge)
		     (limit .charge-limit))  obj
      
      (setf dx 0 dy 0)
      (when up (decf dy speed))
      (when down (incf dy speed))
      (when left (decf dx speed))
      (when right (incf dx speed))
      (when (not (or (zerop dx) (zerop dy)))
	(setf dx (/ dx 1.414)
	      dy (/ dy 1.414)))

      (if (zerop charge)
	  (when space
	    (progn
	      (create player-bullet
		      :x x
		      :y y
		      :speed 10
		      :direction (/ -3.14159 2)
		      :radius 3
		      :color sdl:*blue*)

	      (setf charge limit)))
	  (decf charge)))))


;;; player-bullet
(defclass player-bullet (<circle-shape> <out-to-die> <directed>)
  ())


(relate-collide player enemy)
(relate-collide player enemy-bullet)
(relate-collide player-bullet enemy)


(defparameter *example-game*
  (make-instance 'example
		 :height 640
		 :width 480
		 :player-size 20
		 :player-speed 3
		 :schedule
		 (list 0 
		       #'(lambda ()
			   (create enemy
				   :x 240
				   :y 240
				   :radius 10
				   :color sdl:*red*
				   :speed 0.0
				   :direction 0.0
				   :timer-limit 2
				   :hitpoint 100)))))



