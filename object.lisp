(in-package #:cffi-object)

(declaim (inline make-cobject))
(defstruct cobject
  (pointer (cffi:null-pointer) :type cffi:foreign-pointer :read-only t)
  (shared-from nil :type (or cobject null) :read-only t))

(defun cobject-eq (a b)
  (cffi:pointer-eq (cobject-pointer a) (cobject-pointer b)))

(defun cobject-class-object-size (type)
  (when-let ((type (nth-value 1 (cobject-class-definition type))))
    (cffi:foreign-type-size type)))

(defun pointer-cobject (pointer type)
  (funcall
   (cobject-class-definition-internal-constructor
    (cobject-class-definition type))
   :pointer pointer))

(defun manage-cobject (cobject)
  (let ((pointer (cobject-pointer cobject))
        (deallocator (cobject-allocator-deallocator *cobject-allocator*)))
    (if (eq deallocator #'values) cobject (tg:finalize cobject (lambda () (funcall deallocator pointer))))))

(defun unmanage-cobject (cobject)
  (tg:cancel-finalization cobject)
  (cobject-pointer cobject))

(defgeneric cobject-type (object)
  (:method (object)
    (type-of object)))

(defun cobject-new (cffi-type &optional (cobj-type cffi-type))
  "Allocates an object of the given TYPE and manages it., TYPE should be
a suitable parameter for CFFI:FOREIGN-ALLOC, COBJ-TYPE a suitable
second parameter to COBJ:POINTER-CPOINTER."
  (manage-cobject
   (pointer-cpointer
    (cffi:foreign-alloc cffi-type)
    (or cobj-type cffi-type))))

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

(defun wrap-lvalue (cobj &optional cobj-type)
  "Return a pointer. A conceptual Address-of operation on the returned
value points to the given COBJ"
  (pointer-cpointer
   (cffi:mem-ref (cobject-pointer cobj) :pointer)
   (or cobj-type :pointer)))
