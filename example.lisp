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
	    :x (/ width 2)
	    :y (- height 10 (/ player-size 2))
	    :speed player-speed
	    :radius (/ player-size 2)
	    :color sdl:*white*)))


;;; enemy
(defclass enemy (<circle-shape> <directed> <out-to-die>)
  ())

;;; player
(defclass player (<circle-shape> <directed> <fixed>)
  ((charge :initform 20
	   :accessor .charge)
   (acc :initarg :acc
	:initform 1
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

      (when (< charge 20)
	(incf charge))
      (when (and space (= charge 20))
	(create 'bullet :x x :y (- y r 5) :radius 2 :speed 10 :direction dir)
	(setf charge 0)))))

;;; bullet
(defclass bullet (<circle-shape> <directed> <out-to-die>) ())

(relate-collide player enemy)
(relate-collide bullet enemy)



(defparameter *example-game*
  (make-instance 'example
		 :schedule
		 (list 0
		       #'(lambda ()
			   (iter (repeat 100)
				 (create enemy
					 :x (random *screen-width*)
					 :y (random *screen-height*)
					 :radius (+ 5 (random 20))
					 :direction (random 6.283)
					 :speed (+ 1 (random 2.0))))))))

;; (with-accessors ((x .x) (y .y) (r .radius))  (car shooting.core::*registered-objects*)
;;   (list x y r))
