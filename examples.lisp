(defpackage #:advisor-examples
  (:shadowing-import-from #:advisor #:defun)
  (:import-from #:advisor #:defadvice #:remove-advice 
		#:activate-advice #:deactivate-advice
		#:call-main-function #:call-main-function-with-args)
  (:use #:cl))

(in-package #:advisor-examples)

(defun adder (a b &rest numbers)
  (apply '+ (cons a (cons b numbers))))

(defadvice :before adder (&rest ignore)
  (format t "~&calling adder like so:  (adder ~{~A~^ ~})~%" ignore))

(defun keytest (&key (print-me 'HI))
  (format nil "~A" print-me))

(defadvice :before keytest (&rest args)
  (format t "~&calling keytest like so:  (keytest~A~{~S~^ ~})~%"
	  (if args " " "") args))

(defun wacky-indexer (index list)
  (nthcdr index list))

(defadvice :around wacky-indexer (index list)
  (call-main-function-with-args (+ 1 index) list))
