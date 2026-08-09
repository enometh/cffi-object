(in-package "CFFI-OBJECT")


;;; ----------------------------------------------------------------------
;;;
;;; let-bind cobj:*claw-generate-defcobjfun* to T around a call to
;;; claw.wrapper:generate-bidings to make claw generate spec files
;;; with cobj:defcobjfun instead of cffi:defcfun
;;;

(export 'cffi-object::*claw-generate-defcobjfun* "CFFI-OBJECT")
(defvar *claw-generate-defcobjfun* nil
  "If Non-NIL CLAW.WRAPPER:GENERATE-BINDINGS will generate bindings with
COBJ:DEFCOBJEFUN instead of CFFI:DEFCFUN.")

(defclass cffi-cobj-generator (claw.cffi.c::cffi-generator) ())

(defmethod claw.wrapper:generate-bindings :around ((generator (eql :claw/cffi))
						   (language (eql :c))
						   wrapper
						   configuration)
  (if (not *claw-generate-defcobjfun*)
      (call-next-method)
      (let ((claw.cffi.c::*qualify-records* t))
	(claw.generator.common:explode-library-definition (make-instance 'cffi-cobj-generator) language wrapper configuration))))

(defmethod claw.cffi.c::generate-binding ((generator cffi-cobj-generator) (entity claw.spec:foreign-function) &key)
  (multiple-value-bind (adapted adapted-p)
      (claw.generator.common:adapt-function entity)
    (let* ((id (claw.generator.common:entity->cffi-type entity))
           (c-name (if (and (claw.generator.common:adapter)
                            (or adapted-p
                                (claw.spec:foreign-function-inlined-p entity)))
                       (claw.generator.common:register-adapted-function adapted)
                       (claw.spec:foreign-entity-name entity))))
      (claw.generator.common:export-symbol id)
      `(,@(when claw.generator.common:*inline-functions*
            `((declaim (inline ,id))))
          (cobj:defcobjfun (,c-name ,id) ,(claw.generator.common:entity->cffi-type (claw.generator.common:adapted-function-result-type adapted))
            ,(claw.spec:format-foreign-location
              (claw.spec:foreign-entity-location entity))
            ,@(claw.cffi.c::generate-cffi-parameters (claw.generator.common:adapted-function-parameters adapted))
            ,@(when (claw.spec:foreign-function-variadic-p entity)
		(list 'cl:&rest)))))))

#+nil
(remove-method #'claw.wrapper:generate-bindings
	       (find-method #'claw.wrapper:generate-bindings
			    '(:around)
			    (list '(eql :claw/cffi)
				  '(eql :c)
				  (find-class t)
				  (find-class t))))