(shooting.utils:namespace example-game
  (:use :common-lisp
	:iterate
	:shooting.core
	:shooting.middle)
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
	  *screen-width* width
	  *world* (make-instance '<full-search>
		   :schedule schedule
		   :key-state (make-instance 'key-state)))
    (create player
	    :x (float (/ width 2))
	    :y (float (- height 10 (/ player-size 2)))
	    :speed (float player-speed)
	    :radius (float (/ player-size 2))
	    :color sdl:*white*)))


;;; enemy
(defclass enemy (<circle-shape> <out-to-die> <directed>)
  ())

;;; player
(defclass player (<circle-shape> <fixed> <directed>)
  ((charge :initform 5
	   :accessor .charge)
   (acc :initarg :acc
	:initform 0.05
	:accessor .acc)))

(defmethod update ((obj player))
  (with-slots (up down right left space) (.key-state *world*)
    (with-accessors ((x .x)
		     (y .y)
		     (r .radius)
		     (dir .direction)
		     (acc .acc)
		     (speed .speed)
		     (charge .charge))  obj
      (let ((dx (* speed (cos dir)))
	    (dy (* speed (sin dir))))
	(when up (decf dy acc))
	(when down (incf dy acc))
	(when left (decf dx acc))
	(when right (incf dx acc))

	(setf speed (sqrt (+ (* dx dx) (* dy dy)))
	      dir (atan dy dx)))

      (when (< charge 5)
	(incf charge))
      (when (and space (= charge 5))
	(create bullet :x (+ x (* (+ r 5) (cos dir))) :y (+ y (* (+ r 5) (sin dir))) :radius 2.0 :speed 10.0 :direction dir :color sdl:*blue*)
	(setf charge 0)))))

;;; bullet
(defclass bullet (<circle-shape> <out-to-die> <directed>) ())

(relate-collide player enemy)
(relate-collide bullet enemy)



(defparameter *example-game*
  (make-instance 'example
		 :schedule
		 (iter (for i from 1 to 100)
		       (collect (* 60 i))
		       (collect
			   #'(lambda ()
			       (create enemy
				       :x (* (random 1.0) *screen-width*)
				       :y (* (random 1.0) *screen-height*)
				       :radius (+ 5.0 (random 20.0))
				       :direction (random 6.283)
				       :speed (+ 1.0 (random 2.0))
				       :color sdl:*red*))))
		 :player-speed 0.0))
