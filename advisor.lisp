;;;; advisor.lisp

(in-package #:advisor)

(define-condition advisable-function-error (error) ())
(define-condition no-advisable-function-error (advisable-function-error)
  ((designator :initarg :designator
	       :reader no-advisable-function-error-designator))
  (:report
   (lambda (c s)
     (with-slots (designator) c
       (format s "The symbol ~S doesnt designate an advisable function"
	       designator)))))

(defmacro with-advisable-object ((var symbol) &body body)
  "bind `var` to the advisable-function denoted by `symbol` and execute `body`. 
If `symbol` doesnt denote an advisable-function error out."
  `(let ((,var (gethash ,symbol *advice-hash-table*)))
     (if ,var
	 (progn ,@body)
	 (error 'no-advisable-function-error :designator ',symbol))))

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

(defparameter *advice-hash-table* (make-hash-table)
  "dispatch table for advisable-functions and their registered advice")

(cl:defun make-advisable-function (name main dispatch)
  "create an advisable function object"
  (setf (gethash name *advice-hash-table*)
	(make-instance 'advisable-function
		       :main main
		       :dispatch dispatch)
	(symbol-function name) dispatch))

(defmacro defun (name args &body body)
  "A replacement for cl:defun which creates an advisable-function object if one 
does not exist, or redefines the main function if one does exist."
  (let* ((docstring (and (stringp (car body)) (car body)))
	 (realbody (if docstring (cdr body) body))
	 (g (gensym)) (main (gensym)) (before (gensym)) (after (gensym))
	 (obj (gensym)) (around (gensym)) (arguments (gensym "ARGUMENTS-LIST")))
    `(let ((,g (gethash ',name *advice-hash-table*)))
       (if ,g
	   (with-slots (main) ,g
	     (setf main (lambda ,args
			  ,@(when docstring (list docstring))
			  ,@realbody)))
	   (make-advisable-function
	    ',name
	    (lambda ,args ,@(when docstring (list docstring)) ,@realbody)
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
		  (when ,after (apply ,after ,arguments))))))))))

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
		 (let* ((docstring (when (stringp (car body)) (car body)))
			(realbody (if docstring (cdr body) body)))
		   `((advisable-function-around ,g)
		     (macrolet ((call-main-function ()
				  `(apply (advisable-function-main ,',g)
					  ,',restarg))
				(call-main-function-with-args (&rest callargs)
				  `(apply (advisable-function-main ,',g)
					  (list ,@callargs))))
		       (lambda (&rest ,restarg)
			 ,@(when docstring (list docstring))
			 (destructuring-bind ,args ,restarg
			   ,@realbody))))))))
	   (error 'no-advisable-function-error :designator ',function-name)))))

(defun remove-advice (qualifier name)
  "Remove advice for `name` determined by `qualifier`. Acceptable qualifiers are
:before :after :around :all/:everything."
  (with-advisable-object (advice-object name)
    (case qualifier
      (:around (setf (advisable-function-around advice-object) nil))
      (:before (setf (advisable-function-before advice-object) nil))
      (:after (setf (advisable-function-after advice-object) nil))
      ((:all :everything)
       (setf (advisable-function-around advice-object) nil
	     (advisable-function-before advice-object) nil
	     (advisable-function-after advice-object) nil
	     (symbol-function name) (advisable-function-main advice-object))))))

(defun delete-advice (symbol &optional fmakunbound)
  "Delete an advisable function and reset the symbol-function to the main 
function. If fmakunbound is t, call fmakunbound on symbol"
  (if fmakunbound
      (fmakunbound symbol)
      (with-advisable-object (obj symbol)
	(setf (symbol-function symbol) (advisable-function-main obj))))
  (remhash symbol *advice-hash-table*))

(defun deactivate-advice (symbol)
  "Deactivate advice for a function, replacing the symbol-function with the main
function of the advisable-function object."
  (with-advisable-object (advisable-object symbol)
    (setf (symbol-function symbol) (advisable-function-main advice-object))))

(defun activate-advice (symbol)
  "Activate advice for a function, replacing the symbol-function with the dispatch
function of the advisable-function object"
  (with-advisable-object (advisable-object symbol)
    (setf (symbol-function symbol) (advisable-function-dispatch advice-object))))

(defun advice-documentation (symbol)
  "returns an alist of the documentation for all advices and the main function"
  (let (ret)
    (with-advisable-object (advice-object symbol)
      (with-slots (main before after around) advice-object
	(when after (push (list :after (documentation after t)) ret))
	(push (list :main (documentation main t)) ret)
	(when around (push (list :around (documentation around t)) ret))
	(when before (push (list :before (documentation before t)) ret))
	ret))))

