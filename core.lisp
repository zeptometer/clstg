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
nn  (when source
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


(defmethod collidep ((obj1 <object>) (obj2 <object>) 
		     (trr1 <circle-territory>) (trr2 <circle-territory>))
  (labels ((sqr (x) (* x x)))
    (<= (+ (sqr (- (.x obj1) (.x obj2))) (sqr (- (.y obj1) (.y obj2)))) 
	(sqr (+ (.radius trr1) (.radius trr2))))))


;;; world
(defclass <game> () ())

(defclass <world> () 
  ((key-state :accessor .key-state
	      :initarg :key-state)
   (map :accessor .map
	:initarg :map)
   (current-frame :accessor .current-frame
		  :initform 0)))

(defgeneric create-world (game))
(defgeneric update-world (world))
(defgeneric draw-world (world))

(defmethod update-world ((world <world>)) nil)
(defmethod update-world :before ((world <world>))
  (with-accessors ((map .map) (current-frame .current-frame)) world
    (destructuring-bind (frame event &rest rest) map
      (when (= frame current-frame)
	(funcall event)
	(setf map rest)))
    (incf current-frame)))

(defmethod draw-world ((wrold <world>)) nil)


;;; main loop
(defun main (game)
  (setf *registered-objects* nil)
  (sdl:with-init ()
    (sdl:window *screen-width* *screen-height* :title-caption "test") 
    (setf (sdl:frame-rate) 60) 
    (sdl:initialise-default-font sdl:*font-10x20*)

    (let ((world (create-world game)))
      (sdl:update-display)

      (sdl:with-events ()
	(:quit-event () t)
	(:key-down-event (:key key)
	  (if (sdl:key= key :sdl-key-escape)
	      (sdl:push-quit-event)
	      (update-key-state key t (.key-state world))))
	(:key-up-event (:key key)
	  (update-key-state key nil (.key-state world)))
	(:idle ()
	  (sdl:clear-display sdl:*black*)

	  ;; update world
	  (update-world world)

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
	  
	  ;; draw world
	  (draw-world world)

	  ;; draw alive objects
	  (iter (for obj in *registered-objects*)
		(draw obj))
	  (sdl:update-display))))))
