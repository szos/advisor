;;;; package.lisp

(defpackage #:advisor
  (:use #:cl)
  (:shadow #:defun)
  (:export #:defun
	   #:defadvice
	   #:remove-advice
	   #:call-main-function))
