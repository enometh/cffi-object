(defpackage cffi-object
  (:use #:cl #:alexandria)
  (:nicknames #:cobj)
  (:export #:cobject
           #:cobject-eq
           #:cobject-new
           #:wrap-lvalue
           #:cobject-pointer
           #:cpointer
           #:pointer-cpointer
           #:cref
           #:cpointer-equal
           #:cpointer-eq
           #:carray
           #:make-carray
           #:pointer-carray
           #:cpointer-carray
           #:carray-dimensions
           #:carray-displacement
           #:carray-element-type
           #:ccoerce
           #:caref
           #:clength
           #:creplace
           #:cfill
           #:carray-equal
           #:define-cobject-class
           #:define-global-cobject
           #:*define-global-cobject*
           #:pointer-cobject
           #:manage-cobject
           #:unmanage-cobject
           #:with-monotonic-buffer-allocator
           #:with-default-allocator
           #:with-leaky-allocator
           #:defcobjfun
           #:*sorted-symbol-list*
           #:turn-on-cobj
           #:turn-off-cobj
   ))

(in-package #:cffi-object)
