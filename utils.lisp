;;; -*- Mode: LISP; Package: :cl-user; BASE: 10; Syntax: ANSI-Common-Lisp; -*-
;;;
;;;   Time-stamp: <>
;;;   Touched: Fri Mar 27 17:49:26 2026 +0530 <enometh@net.meer>
;;;   Bugs-To: enometh@net.meer
;;;   Status: Experimental.  Do not redistribute
;;;   Copyright (C) 2026 Madhu.  All Rights Reserved.
;;;
;;;
;;; utils.lisp: collect experimental code here
;;;
(in-package "CFFI-OBJECT")

;;; ----------------------------------------------------------------------
;;;
;;;
;;;
(export '(cobject-new))

(defun cobject-new (cffi-type &optional (cobj-type cffi-type))
  "Allocates an object of the given CFFI-TYPE and manages it.
CFFI-TYPE should be a suitable parameter for CFFI:FOREIGN-ALLOC,
COBJ-TYPE a suitable second parameter to COBJ:POINTER-CPOINTER."
  (manage-cobject
   (let ((tclass (cffi::parse-type cffi-type)))
     (pointer-cpointer
      (funcall (cobject-allocator-allocator *cobject-allocator*)
	       tclass)
      (or cobj-type cffi-type)))))

#||
(cffi:defcstruct foo (a :int) (b :int))
(cobj:define-cobject-class foo)
(setq $f (cobject-new 'foo))
(setf (foo-a $f) 10)
(setf (foo-b $f) 10)
(cffi:foreign-slot-value (cobj:cobject-pointer $f) 'foo 'a)
(setq $fl (wrap-lvalue $f))
(cffi:pointer-eq (cffi:mem-ref (cobj:cobject-pointer $f) :pointer)
		 (cobj:cobject-pointer $fl))
||#


;;; ----------------------------------------------------------------------
;;;
;;;
;;;
(export '(wrap-lvalue))

(defun wrap-lvalue (cobj &optional cobj-type)
  "Return a pointer. A conceptual Address-of operation on the returned
value points to the given COBJ"
  (pointer-cpointer
   (cffi:mem-ref (cobject-pointer cobj) :pointer)
   (or cobj-type :pointer)))


;;; ----------------------------------------------------------------------
;;;
;;;
;;;
(export '(cobj-definables get-exportables))

(defun cobj-definables (spec-package)
  (loop for s being each symbol of spec-package
	with defn
	when (and (find-class s nil)
		  (setq defn (ignore-errors
			       (cobj::cobject-class-definition s))))
	collect defn))

(defun get-exportables (spec-package &key include-already-exported restrict-to-internal-syms)
  (loop for defn in (cobj-definables spec-package)
	append
	(remove-if
	 (lambda (s)
	   (multiple-value-bind (sym stat)
	       (find-symbol (string s) spec-package)
	     (assert (eql sym s))
	     (and (or (not restrict-to-internal-syms)
		      (eql (symbol-package sym) (find-package spec-package)))
		  (unless include-already-exported
		    (eql stat :external)))))
	 (cobj::cobject-class-definition-symbols defn))))


;;; ----------------------------------------------------------------------
;;;
;;;
;;;
(export '(null-cpointer null-cpointer-p))

(cffi:defcstruct (null-ptr :size 0))
(define-cobject-class (:struct null-ptr))

(defun null-cpointer ()
  (pointer-cobject (cffi:null-pointer) 'null-ptr))
(defun null-cpointer-p (c)
  (cffi:null-pointer-p (cobject-pointer c)))

#||
(defvar +cobj-null+ (null-cpointer))
(null-cpointer-p +cobj-null+)
||#