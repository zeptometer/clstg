(shooting.utils:namespace shooting.core
  (:use :common-lisp
	:iterate)
  (:export :*screen-width*
	   :*screen-height*

	   :defkeystate
	   :register-object
	   :discard

	   :<territory>
	   :<circle-territory>

	   :<object>
	   :.x
	   :.y
	   :.alivep
	   
	   :create
	   :update
	   :draw
	   :collidablep
	   :colldep

	   :<game>
	   :height
	   :width
	   :schedule

	   :<world>
	   :.key-state
	   
	   :initialize-game
	   :update-world
	   :draw-world

	   :start-game))

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
   (alivep :initform t
	   :accessor .alivep)
   (territory :type <territory>
	      :initarg :territory
	      :accessor .territory)))

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

(defmethod collidep ((obj1 <object>) (obj2 <object>) 
		     (trr1 <circle-territory>) (trr2 <circle-territory>))
  (labels ((sqr (x) (* x x)))
    (<= (+ (sqr (- (.x obj1) (.x obj2))) (sqr (- (.y obj1) (.y obj2)))) 
	(sqr (+ (.radius trr1) (.radius trr2))))))


(defun register-object (obj)
  (declare (type <object> obj))
  (push obj *tobe-registered-objects*))

(defun discard (obj)
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
(defgeneric draw-world (world))

(defmethod update-world ((world <world>)) nil)
(defmethod update-world :before ((world <world>))
  (with-accessors ((schedule .schedule) (current-frame .current-frame)) world
    (when schedule
      (destructuring-bind (frame event &rest rest) schedule
	(when (= frame current-frame)
	  (funcall event)
	  (setf schedule rest))))
    (incf current-frame)))

(defmethod draw-world ((wrold <world>)) nil)


;;; main loop
(defun start-game (game)
  (setf *registered-objects* nil)
  (let ((world (initialize-game game)))
    (sdl:with-init ()
      (sdl:window *screen-width* *screen-height* :title-caption "test") 
      (setf (sdl:frame-rate) 60) 
      (sdl:initialise-default-font sdl:*font-10x20*)

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
		     (update obj world))

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
