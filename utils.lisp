(defpackage shooting.utils
  (:use :common-lisp)
  (:export :namespace
	   :with-gensyms))

(in-package shooting.utils)

(defmacro namespace (name &rest options)
  `(progn
     (in-package :common-lisp)
     (defpackage ,name
       ,@options)
     (in-package ,name)))

(defmacro with-gensyms (syms &body body)
  `(let ,(loop :for sym :in syms
	    :collect `(,sym ',(gensym (symbol-name sym))))
     ,@body))
