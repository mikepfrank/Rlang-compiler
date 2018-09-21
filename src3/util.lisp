;;; -*- Package: user -*-
(in-package "USER")
;;;----------------------------------------------------------------------
;;; General utilities.

;;;----------------------------------------------------------------------
;;; Some abbreviations for Common Lisp entities.

;; Don't you agree that MULTIPLE-VALUE-BIND's name is too long?
(defmacro mvbind (&rest args)
  `(multiple-value-bind . ,args))

;; Same here.
(defmacro dbind (&rest args)
  `(destructuring-bind . ,args))

;; IFMATCH - If the given FORM matches the given LAMBDA-LIST, then
;; execute BODY in a context where the variables in LAMBDA-LIST are
;; lexically bound to the corresponding elements of FORM.  Otherise,
;; just return NIL.  Differs from DBIND in that errors are ignored.

(defmacro ifmatch (form lambda-list &body body)
  `(ignore-errors
    (dbind ,lambda-list ,form . ,body)))

;Another possible implementation:
;  (handler-case
;    (dbind ,lambda-list ,form . ,body)))
;    (error (ignored))))

;;;----------------------------------------------------------------------
;;; Boolean shtuff.  Silly, but hey.

(defconstant true t)
(defconstant false nil)

(defmacro true! (&rest places)
  `(setf . ,(mapcan #'(lambda (place) (list place true)) places)))
(defmacro false! (&rest places)
  `(setf . ,(mapcan #'(lambda (place) (list place false)) places)))

;; Convert an arbitrary object to a true-false value.
(defun true? (obj) (if obj true false))
(defun false? (obj) (eq obj false))

;;;----------------------------------------------------------------------
;;; List manipulation.

;; REPL - Replace first occurrence.  FUNC is called on each item of LIST in
;; succession until it returns non-NIL, at which point a new list is
;; returned in which the guilty item is replaced by the value which was
;; returned by FUNC.  The new list shares its tail with the old.  If FUNC
;; never returns non-nil then a copy of LIST is returned.
(defun repl (list func)
  (if list
      (let ((v (funcall func (car list))))
	(if v (cons v (cdr list))
	  (cons (car list) (repl (cdr list) func))))))

;; In this version of REPL, FUNC is passed not only each item of LIST,
;; but also the item's index (as per NTH or ELT).
(defun repl2 (list func &optional (firstindex 0))
  (labels
      ((helper (list index func)
	  (if list
	      (let ((v (funcall func (car list) index)))
		(if v (cons v (cdr list))
		  (cons (car list) (helper (cdr list) (1+ index) func)))))))
    (helper list firstindex func)))

