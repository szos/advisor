;;;; advisor.lisp

(in-package #:advisor)

(defclass advisable-function ()
  ((dispatch :initarg :dispatch :initform nil
	     :accessor advisable-function-dispatch)
   (main   :initarg :main   :initform nil
	   :accessor advisable-function-main)
   (before :initarg :before :initform nil
	   :accessor advisable-function-before)
   (after  :initarg :after  :initform nil
	   :accessor advisable-function-after)
   (around :initarg :around :initform nil
	   :accessor advisable-function-around)))

(defparameter *advice-hash-table* (make-hash-table))

(defmacro defun (name args &body body)
  "A replacement for cl:defun which, instead of defining a function, defines a 
advisable-function object, and additionally defines a dispatch function."
  (let* ((docstring (and (stringp (car body)) (car body)))
	 (realbody (if docstring (cdr body) body))
	 (fn (gensym "FUNCTION-NAME")) (dispatch (gensym "DISPATCH"))
	 (g (gensym)) (main (gensym)) (before (gensym)) (after (gensym))
	 (obj (gensym)) (around (gensym))
	 (arguments (gensym "ARGUMENTS-LIST")))
    `(let ((,g (gethash ',name *advice-hash-table*))
	   (,fn (lambda ,args ,@(when docstring (list docstring)) ,@realbody))
	   (,dispatch
	    (lambda (&rest ,arguments)
	      ,@(when docstring (list docstring))
	      (let* ((,obj (gethash ',name *advice-hash-table*))
		     (,main (advisable-function-main ,obj))
		     (,before (advisable-function-before ,obj))
		     (,after (advisable-function-after ,obj))
		     (,around (advisable-function-around ,obj)))
		(prog2 (when ,before (apply ,before ,arguments))
		    (if ,around
			(apply ,around ,arguments)
			(apply ,main ,arguments))
		  (when ,after (apply ,after ,arguments)))))))
       (if ,g
	   (setf (advisable-function-main ,g) ,fn)
	   (progn
	     (setf (gethash ',name *advice-hash-table*)
		   (make-instance 'advisable-function
				  :main ,fn
				  :dispatch ,dispatch))
	     (setf (symbol-function ',name) ,fn))))))

(defmacro defadvice (qualifier function-name args &body body)
  "Defines and registers advice for `function-name`. Acceptable qualifiers are 
:after :before :around. When defining an :around advice, the main function is 
exposed via the local function `call-main-function`. If `call-main-function` isnt
called within the body of the :around advice, it wont be called. :before and 
:after advice is called before and after :around advice (or the main function)
respectively. "
  (let ((g (gensym))
	(restarg (gensym)))
    `(let ((,g (gethash ',function-name *advice-hash-table*)))
       (unless (equal (symbol-function ',function-name)
		      (advisable-function-dispatch ,g))
	 (setf (symbol-function ',function-name)
	       (advisable-function-dispatch ,g)))
       (if ,g
	   (setf
	    ,@(case qualifier
		(:after
		 `((advisable-function-after ,g)
		   (lambda ,args ,@body)))
		(:before
		 `((advisable-function-before ,g)
		   (lambda ,args ,@body)))
		(:around
		 `((advisable-function-around ,g)
		   (macrolet ((call-main-function ()
				`(apply (advisable-function-main ,',g)
					,',restarg))
			      (call-main-function-with-args (&rest callargs)
				`(apply (advisable-function-main ,',g)
					(list ,@callargs))))
		     (lambda (&rest ,restarg)
		       (destructuring-bind ,args ,restarg
			 ,@body)))))))
	   (error "Not an advisable function")))))

(defun remove-advice (qualifier name)
  "Remove advice for `name` determined by `qualifier`. Acceptable qualifiers are
:before :after :around :all/:everything."
  (let ((advice-object (gethash name *advice-hash-table*)))
    (case qualifier
      (:around (setf (advisable-function-around advice-object) nil))
      (:before (setf (advisable-function-before advice-object) nil))
      (:after (setf (advisable-function-after advice-object) nil))
      ((:all :everything)
       (setf (advisable-function-around advice-object) nil
	     (advisable-function-before advice-object) nil
	     (advisable-function-after advice-object) nil
	     (symbol-function name) (advisable-function-main advice-object))))))

(defun deactivate-advice (symbol)
  (let ((advice-object (gethash symbol *advice-hash-table*)))
    (if advice-object
	(setf (symbol-function symbol) (advisable-function-main advice-object))
	(error "No advice object found"))))

(defun activate-advice (symbol)
  (let ((advice-object (gethash symbol *advice-hash-table*)))
    (if advice-object
	(setf (symbol-function symbol)
	      (advisable-function-dispatch advice-object))
	(error "No advice object found"))))

