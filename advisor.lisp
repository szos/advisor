;;;; advisor.lisp

(in-package #:advisor)

(defclass advisable-function ()
  ((main   :initarg :main   :initform nil
	   :accessor advisable-function-main)
   (before :initarg :before :initform nil
	   :accessor advisable-function-before)
   (after  :initarg :after  :initform nil
	   :accessor advisable-function-after)
   (around :initarg :around :initform nil
	   :accessor advisable-function-around)))

(defparameter *advice-hash-table* (make-hash-table))

(defmacro defun (name args &body body)
  "A wrapper around cl:defun which, instead of defining a function, defines a 
advisable-function object, and additionally defines a dispatch function."
  (let* ((docstring (and (stringp (car body)) (car body)))
	 (realbody (if docstring (cdr body) body))
	 (fn (gensym "FUNCTION-NAME"))
	 (g (gensym)) (main (gensym)) (before (gensym)) (after (gensym))
	 (obj (gensym)) (around (gensym)))
    `(let ((,g (gethash ',name *advice-hash-table*))
	   (,fn (lambda ,args ,@(when docstring (list docstring)) ,@realbody)))
       (if ,g
	   (setf (advisable-function-main ,g) ,fn)
	   (setf (gethash ',name *advice-hash-table*)
		 (make-instance 'advisable-function :main ,fn)))
       (cl:defun ,name (&rest arguments)
	 ;; ,args
	 ,@(when docstring (list docstring))
	 (let* ((,obj (gethash ',name *advice-hash-table*))
		(,main (advisable-function-main ,obj))
		(,before (advisable-function-before ,obj))
		(,after (advisable-function-after ,obj))
		(,around (advisable-function-around ,obj)))
	   (prog2 (when ,before (apply ,before arguments))
	       (if ,around
		   (apply ,around arguments)
		   (apply ,main arguments))
	     (when ,after (apply ,after arguments))))))))

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
					,',restarg)))
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

