(shooting.utils:namespace shooting.core
  (:use :common-lisp
	:iterate)
  (:export :*world*
	   :*screen-width*
	   :*screen-height*

	   :defkeystate

	   :<territory>

	   :<object>
	   :.x
	   :.y
	   :.territory
	   :.alivep
	   :create
	   :register-object
	   :update
	   :draw
	   :relate-collide
	   :collidablep
	   :collidep
	   :collide
	   :discard

	   :<game>
	   :height
	   :width
	   :schedule

	   :<world>
	   :.key-state
	   :update-world
	   :draw-world
	   
	   :initialize-game
	   :start-game))

;;;variables
(defparameter *world* "represents the world of the game. It contains information of the game, controls detect-collision, and so on.")
(defparameter *screen-width* 640)
(defparameter *screen-height* 480)

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

;;; object
(defclass <object> ()
  ((x :type float
      :initarg :x
      :accessor .x)
   (y :type float
      :initarg :y
      :accessor .y)
   (alivep :initform t
	   :accessor .alivep)
   (territory :type <territory>
	      :initarg :territory
	      :accessor .territory)))

(defmacro create (name &rest initargs)
  "make object corresponds to NAME and enter it to *world*"
  `(register-object (make-instance ',name ,@initargs) *world*))
(defgeneric register-object (obj world)
  (:documentation "enter given OBJ to WORLD."))
(defgeneric update (obj)
  (:documentation "update OBJ"))
(defmethod update ((obj <object>)) nil)
(defgeneric draw (obj)
  (:documentation "draw OBJ"))
(defgeneric collidablep (obj1 obj2))
(defmethod collidablep ((obj1 <object>) (obj2 <object>))
  nil)
(defmacro relate-collide (type1 type2)
  (shooting.utils:with-gensyms (obj1 obj2)
    (if (eql type1 type2)
	`(defmethod collidablep ((,obj1 ,type1) (,obj2 ,type2)) t)
	`(progn 
	   (defmethod collidablep ((,obj1 ,type1) (,obj2 ,type2)) t)
	   (defmethod collidablep ((,obj1 ,type2) (,obj2 ,type1)) t)))))
(defgeneric collidep (obj1 obj2 trr1 trr2))
(defmethod collidep (obj1 obj2 (trr1 <territory>) (trr2 <territory>))
  nil)
(defgeneric collide (collidee collider))
(defmethod collide ((collidee <object>) (collider <object>))
  (discard collidee))
(defgeneric discard (obj))
(defmethod discard ((obj <object>)) nil)
(defmethod discard :after ((obj <object>))
  (setf (.alivep obj) nil))

;;; world
(defclass <game> ()
  ((height :initarg :height
	   :initform 480)
   (width :initarg :width
	  :initform 640)
   (schedule :initarg :schedule
	     :initform nil)))

(defclass <world> () 
  ((key-state :accessor .key-state
	      :initarg :key-state)
   (schedule :accessor .schedule
	     :initarg :schedule
	     :initform nil)
   (current-frame :accessor .current-frame
		  :initform 0)))

(defgeneric initialize-game (game))

(defgeneric update-world (world))
(defmethod update-world ((world <world>)) nil)
(defmethod update-world :before ((world <world>))
  (with-accessors ((schedule .schedule) (current-frame .current-frame)) world
    (when schedule
      (destructuring-bind (frame event &rest rest) schedule
	(when (= frame current-frame)
	  (funcall event)
	  (setf schedule rest))))
    (incf current-frame)))

(defgeneric draw-world (world))
(defmethod draw-world ((world <world>)) nil)


;; main loop
(defun start-game (game)
  (sdl:with-init ()
    (initialize-game game)

    (sdl:window *screen-width* *screen-height* :title-caption "test")
    (setf (sdl:frame-rate) 60) 
    (sdl:initialise-default-font sdl:*font-10x20*)

    (sdl:update-display)

    (sdl:with-events ()
      (:quit-event () t)
      (:key-down-event (:key key)
		       (if (sdl:key= key :sdl-key-escape)
			   (sdl:push-quit-event)
			   (update-key-state key t (.key-state *world*))))
      (:key-up-event (:key key)
		     (update-key-state key nil (.key-state *world*)))
      (:idle ()
	     (sdl:clear-display sdl:*black*)

	     (update-world *world*)
	     (draw-world *world*)

	     (sdl:update-display)))))
