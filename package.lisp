;;;; package.lisp

(defpackage #:advisor
  (:use #:cl)
  (:shadow #:defun)
  (:export #:defun
	   #:defadvice
	   #:call-main-function
	   #:call-main-function-with-args
	   #:remove-advice
	   #:activate-advice
	   #:deactivate-advice))
