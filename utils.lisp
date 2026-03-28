(in-package "CFFI-OBJECT")
(export '(cobj-definables get-exportables))

(defun cobj-definables (spec-package)
  (loop for s being each symbol of spec-package
	with defn
	when (and (find-class s nil)
		  (setq defn (ignore-errors
			       (cobj::cobject-class-definition s))))
	collect defn))

(defun get-exportables (spec-package)
  (loop for defn in (cobj-definables spec-package)
	append
	(remove-if
	 (lambda (s)
	   (multiple-value-bind (sym stat)
	       (find-symbol (string s) spec-package)
	     (assert (eql sym s))
	     (assert (eql (symbol-package sym) (find-package spec-package)))
	     (eql stat :external)))
	 (cobj::cobject-class-definition-symbols defn))))
