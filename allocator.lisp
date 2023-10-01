(in-package #:cffi-object)

(defstruct foreign-allocator
  (allocator (constantly (cffi:null-pointer)) :type (function (non-negative-fixnum) (values cffi:foreign-pointer)))
  (deallocator #'values :type (function (cffi:foreign-pointer))))

(declaim (type foreign-allocator *foreign-allocator*))
(defparameter *foreign-allocator* (make-foreign-allocator :allocator #'cffi-sys:%foreign-alloc :deallocator #'cffi-sys:foreign-free))

(declaim (inline %make-sized-monotonic-buffer-allocator))
(defstruct (sized-monotonic-buffer-allocator (:include foreign-allocator) (:constructor %make-sized-monotonic-buffer-allocator))
  (pointer (cffi:null-pointer) :type cffi:foreign-pointer)
  (size 0 :type non-negative-fixnum)
  (offset 0 :type non-negative-fixnum))

(declaim (inline make-sized-monotonic-buffer-allocator))
(defun make-sized-monotonic-buffer-allocator (&key (pointer (cffi:null-pointer)) (size 0))
  (let* ((upstream-allocator *foreign-allocator*)
         (allocator-1 nil)
         (allocator-2 (%make-sized-monotonic-buffer-allocator :allocator (lambda (size)
                                                                           (declare (type non-negative-fixnum size))
                                                                           (with-accessors ((offset sized-monotonic-buffer-allocator-offset)
                                                                                            (buffer-size sized-monotonic-buffer-allocator-size)
                                                                                            (pointer sized-monotonic-buffer-allocator-pointer)
                                                                                            (allocator sized-monotonic-buffer-allocator-allocator)
                                                                                            (deallocator sized-monotonic-buffer-allocator-deallocator))
                                                                               allocator-1
                                                                             (if (<= (+ offset size) buffer-size)
                                                                                 (prog1 (cffi:inc-pointer pointer offset)
                                                                                   (incf offset size))
                                                                                 (prog1 (funcall (foreign-allocator-allocator upstream-allocator) size)
                                                                                   (setf offset buffer-size)
                                                                                   (setf deallocator (foreign-allocator-deallocator upstream-allocator))))))
                                                              :deallocator #'values :size size :pointer pointer)))
    (setf allocator-1 allocator-2)
    allocator-2))

(defmacro with-monotonic-buffer-allocator ((&key (buffer-size 128)) &body body)
  (with-gensyms (buffer pointer size allocator)
    `(let* ((,size ,buffer-size)
            (,buffer (cffi:make-shareable-byte-vector ,size)))
       (declare (dynamic-extent ,buffer))
       (cffi:with-pointer-to-vector-data (,pointer ,buffer)
         (let ((,allocator (make-sized-monotonic-buffer-allocator :pointer ,pointer :size ,size)))
           (declare (dynamic-extent ,allocator))
           (let ((*foreign-allocator* ,allocator))
             ,@body))))))
