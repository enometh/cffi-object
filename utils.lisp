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


;;; ----------------------------------------------------------------------
;;;
;;;
;;;
(export '(make-cobj-string))

(defun make-cobj-string (str)
  "make-cobj-string (str) replaces (with-foreign-string (s str :encoding utf-8)) but loses the ability to specify explicit utf-8 translation"
  (cobj:make-carray (1+ (length str)) :element-type 'character :initial-contents
		    str))

#||
(setq $s1 (make-cobj-string "Sans Serif"))
(setf (cobj:caref $s1 3) #\c)
(cobj:ccoerce $s1 'string)
||#


;;; ----------------------------------------------------------------------
;;;
;;;
;;;
(export '(with-cobj with-cobjs))

(defun frobp (ptr cffi-type cobj-type)
  "Internal. Second return value is a cffi pointer which needs to be
deallocated if non-NULL. First return value is a cpointer of type (or
cobj-type cffi-type). PTR can be NIL (a cobject is allocated and
returned as the first return value), or a cffi pointer, which is used
to construct the first return value, or a cpointer, which is just returned."
  (let* ((p nil)
	 (pobj
	  (cond ((null ptr)
		 (let ((tclass (cffi::parse-type cffi-type)))
		   (setq p
			 (funcall (cobject-allocator-allocator *cobject-allocator*)
				  tclass)))
		 (pointer-cpointer p (or cobj-type cffi-type)))
		((cffi:pointerp ptr)
		 (pointer-cpointer ptr (or cobj-type cffi-type)))
		((cpointer-p ptr) ptr)
		(t (error "Invalid ptr wanted one of cffi pointer or cffi-object:cpointer on NIL")))))
    (values pobj p)))

(defun call-with-cobj (cffi-type func &key ptr cobj-type)
  (let (p pobj)
    (unwind-protect
	 (progn
	   (multiple-value-setq (pobj p)
	     (frobp ptr cffi-type cobj-type))
	   (funcall func pobj))
      (when p
	(assert (not ptr))
	(funcall (cobject-allocator-deallocator *cobject-allocator*) p)))))

(defmacro with-cobj ((var cffi-type &key ptr cobj-type)  &body body)
  `(flet ((doit (,var) ,@body))
     (call-with-cobj ,cffi-type #'doit :ptr ,ptr :cobj-type ,cobj-type)))

(defun call-with-cobjs (bindings-specs function)
  "BINDINGS-SPECTS = (CFFI-TYPE &KEY PTR COBJ-TYPE)"
  ;;(declare (optimize (safety 3) (debug 3)))
  (let (specs)
    (unwind-protect
	 (progn (dolist (bind bindings-specs)
		  (destructuring-bind (cffi-type &key ptr cobj-type) bind
		    (multiple-value-bind (pobj p)
			(frobp ptr cffi-type cobj-type)
		      (push (list pobj p) specs))))
		(let ((params  (reverse (mapcar #'car specs))))
		  (apply function params)))
      (loop for (pobj p) in specs
	    when p
	    do (funcall (cobject-allocator-deallocator *cobject-allocator*)
			p)))))

(declaim (inline call-with-cobjs call-with-cobj))

(defmacro with-cobjs (bindings &body body)
  (let ((args (mapcar #'car bindings))
	(specs (mapcar #'cdr bindings)))
    `(flet ((doit ,args ,@body))
       ;;(declare (optimize (speed 0) (safety 3) (debug 3)))
       (call-with-cobjs
	(list ,@(loop for b in specs
		      collect (destructuring-bind (cffi-type &key ptr cobj-type) b
				`(list ',cffi-type :ptr ,ptr :cobj-type ',cobj-type))))
	#'doit))))

#||
(cffi:defcstruct foo (a :int) (b :int))
(cobj::define-struct-cobject-class foo)
(setq $foo-1 (cobj:cobject-new 'foo))

(time
 (with-cobjs ((foo (:struct foo) :ptr $foo-1)
	      (bar (:struct foo)))
   (setf (foo-a foo) 10)
   (setf (foo-b foo) 20)
   (setf (foo-b bar) 30)
   (setf (foo-a bar) 40)
   (list foo bar)))
(call-with-cobjs `(((:struct foo) :ptr ,$foo-1))
		 #'(lambda (x)
		     (setf (foo-a x) 10)
		     (setf (foo-b x) 20)
		     x))
(list (foo-a $foo-1) (foo-b $foo-1))
(call-with-cobj '(:struct foo)
		#'(lambda (x)
		    (setf (foo-a x) 10)
		    (setf (foo-b x) 20)
		    x))
(with-cobj (foo-1 '(:struct foo) :ptr $foo-1)
  (setf (foo-a foo-1) 65))
||#