;;; -*- Package: user -*-
(in-package "USER")

;; This version of WITH allows any temporary effect, not just
;; variable binding, to be done and undone around the body.
(defconstruct with (statement &body body)
  `(,statement
    ,@body
    (undo ,statement)))

;; Given any statements, do their reverse (undoing their effects).  Don't
;; depend too much on this always working yet.
(defconstruct undo (&rest statements)
  (unless (null statements)
    (dbind (first . rest) statements
      (if (list-of-statements? first)
	  `(undo ,@first . ,rest)
	(let ((statement (canonicalize-form first)))
	  `((undo . ,rest)
	    (,(opposite (first statement)) . ,(rest statement))))))))

;; Maps "expanding" binary operators to their do/undo statements. 
(defparameter *expanding*
  '((& ^=&) (<< ^=<<) (>> ^=>>) (* +=* -=*) (*/ +=*/ -=*/) (_ <-_ ->_)))
(defun forw (binop)
  (cadr (assoc binop *expanding*)))
(defun revers (binop)
  (or (caddr (assoc binop *expanding*))
      (cadr (assoc binop *expanding*))))

(defun expression? (obj)
  (and (listp obj)
       (not (location? obj))
       (not (statement? obj))))

;; This version of with doesn't evaluate expressions as many times.
;; but uses up linear space during body.  It only handles <- (bind) type
;; statements though.
(defconstruct _with ((var <- val) &body body)
  (cond
   ((or (not (expression? val))
	(literal? val env))
    `(with (,var <- ,val)
	   . ,body))
   ((null (cddr val)) ; No more than 2 words in value expression.
    (cond
     ((eq (first val) '*)
      ;; These expansions are a bit questionable because what if the body
      ;; tries to look at the dereferenced value also?  It will see 0 (or
      ;; whatever was in VAR) instead.  But the alternative, of introducing
      ;; yet another temporary and swapping the contents back before doing
      ;; the body, seems too inefficient.
      (if (or (register? (second val))
	      (dynamic-var? (second val) env))
	  `((,var <->* ,(second val))
	    ,@body
	    (,var <->* ,(second val)))
	(let ((tv (gentemp)))
	  `(_with (,tv <- ,(second val))
	      `((,var <->* ,tv)
		,@body
		(,var <->* ,tv))))))))
   (t
    (dbind (a1 ~ a2) val  ;But what about other expressions?
      (let ((rb (revers ~))
	    (fb (forw ~)))
	(cond
	 ((and (numberp a1) (numberp a2))
	  `((,var <- ,(funcall ~ a1 a2)) ;Warning: this is too simplistic.
	    ,@body
	    (,var -> ,(funcall ~ a1 a2))))
	 ((eq ~ '+)
	  `(with ((,var += ,a1)
		  (,var += ,a2))
	    ,@body))
	 ((and (eq ~ '+) (not (expression? a1)))
	  `(_with (,var <- ,a2)
	      (with (,var += a1)
		 ,@body)))
	 ((and (eq ~ '+) (not (expression? a2)))
	  `(_with (,var <- ,a1)
	      (with (,var += ,a2)
		 ,@body)))
	 ((and (eq ~ '-) (not (expression? a2)))
	  `(_with (,var <- ,a1)
	      (with (,var -= ,a2)
		 ,@body)))
	 ((and (expression? a1) (expression? a2))
	  (let ((tv1 (gentemp))
		(tv2 (gentemp)))
	    `(let (,tv1 <- ,a1)
	       (let (,tv2 <- ,a2)
		 (,var ,fb ,tv1 ,tv2)
		 ,@body
		 (,var ,rb ,tv1 ,tv2)))))
	 ((expression? a1)
	  (let ((tv (gentemp)))
	    `(let (,tv <- ,a1)
	       (,var ,fb ,tv ,a2)
	       ,@body
	       (,var ,rb ,tv ,a2))))
	 ((expression? a2)
	  (let ((tv (gentemp)))
	    `(let (,tv <- ,a2)
	       (,var ,fb ,a1 ,tv)
	       ,@body
	       (,var ,rb ,a1 ,tv))))
	 (t
	  `((,var ,fb ,a1 ,a2)
	    ,@body
	    (,var ,rb ,a1 ,a2)))))))))

;;;----------------------------------------------------------------------
;;; Variable binding and unbinding.  For most purposes this takes the
;;; place of assignment.

;; Semantics of BIND: assuming that VAR is already clear,
;; set it to VAL.  
(definfix (var <- val) :opposite ->
  ;; Implemented using +=, but ^= would also work.
  (cond
   ((or (symbolp val) (numberp val) (register? val) (literal? val env))
    `(,var += ,val))
   ((expression? val)
    ;; Binary expression.
    (destructuring-bind (a1 ~ a2) val	;What about other syntaxes?
      (cond
       ((and (numberp a1) (numberp a2))
	`(,var += ,(funcall ~ a1 a2)))	; Really too simplistic.
       ((eq ~ '+)
	`((,var += ,a1)
	  (,var += ,a2)))
       ((eq ~ '-)
	`((,var += ,a1)
	  (,var -= ,a2)))
       ((eq ~ '^)
	`((,var ^= ,a1)
	  (,var ^= ,a2)))
       ((eq ~ '*)
	`(,var +=* ,a1 ,a2))
       ((assoc ~ *expanding*)
	`(,var ,(forw ~) ,a1 ,a2))
       ((extract form env :lvalues 1))
       (t
	(error "Don't know how to compile ~s." form)))))))

;; Assuming that VAR=VAL, restore it to zero.
(definfix (var -> val) :opposite <-
  (cond
   ((or (symbolp val) (numberp val) (register? val) (literal? val env))
    `(,var -= ,val))
   ((expression? val)
    ;; Binary expression.
    (destructuring-bind (a1 ~ a2) val
      (cond
       ((and (numberp a1) (numberp a2))
	`(,var -= ,(funcall ~ a1 a2)))	; Really too simplistic.
       ((eq ~ '+)
	`((,var -= ,a1)
	  (,var -= ,a2)))
       ((eq ~ '-)
	`((,var -= ,a1)
	  (,var += ,a2)))
       ((eq ~ '^)
	`((,var ^= ,a1)
	  (,var ^= ,a2)))
       ((eq ~ '*)
	`(,var -=* ,a1 ,a2))
       ((assoc ~ *expanding*)
	;; Use the appropriate reverse op if different from forward one.
	`(,(revers ~) ,var ,a1 ,a2))
       ((extract form env :lvalues 1))
       (t
	(error "Don't know how to compile ~s." form)))))))
       

;;; ----------------------------------------------------------------------
;;; New thingy.  constructs all use this same function EXTRACT to
;;; automatically replace located variables with their locations, move
;;; stack variables into registers before operating on them, create
;;; temporary variables for subexpressions and compute their values.

;; EXTRACT - you give it a form, and it returns code that
;; creates appropriate temporary stuff around it and gets variables
;; in registers together with a reduced version of the original
;; form.
;;    RELEVANT-TERMS is a list of the indices (as per NTH or ELT) of those
;; terms that are candidates for expanding.  If not provided, all terms are
;; considered fair game.
;;    LVALUES is similarly the index of or a list of the indices of
;; a term or terms that are considered to be "lvalues", that is,
;; "destinations" where the value of the term is changed by the
;; statement.  Anything in LVALUES is automatically also a candidate
;; for expanding.
;;    The indices in both RELEVANT-TERMS and LVALUES refer to the
;; indices the terms have *after* any canonicalization.
;;    NIL is returned if EXTRACT can't do anything.

(defun extract (form env &key lvalues (relevant-terms
				       (labels ((countlist (n)
						 (if (>= n 0)
						     (cons n
						       (countlist
							(1- n))))))
					 (countlist (length form)))))
  (setf form (canonicalize-form form));So we can forget about infix.
  (labels ((lvalue? (index)
	     "Return non-NIL if the given index is the index of
              a term that is an LVALUE (modifiable term)."
	     (or (eql index lvalues)
		 (and (listp lvalues)
		      (member index lvalues))))
	   (candidate? (index)
	     "Return non-NIL if the given index is the index of a
              term that is a candidate for expansion."
	     (or (member index relevant-terms)
		 (lvalue? index))))
    ;; First we locate the first term that is an expression or
    ;; a literal, and convert it into a temporary variable.
    (let (before)
      (setf (cdr form)
	    (repl2 (cdr form)
		  #'(lambda (term index)
		      (if (and (candidate? index)
			       (or (expression? term)
				   (literal? term env)))
			  (let ((tv (gentemp)))
			    (if (lvalue? index)
				(setf before `(,tv <-> ,term))
			      (setf before `(,tv <- ,term)))
			    tv)))
		  1))
      (when before
	(return-from extract `(let ,before ,form))))
    ;; If any term is a static value identifier, wrap the statement in
    ;; a binding of a temporary to the value's address and replace the
    ;; term with a dereferencing expression.
    (let (before)
      (setf (cdr form)
	    (repl2 (cdr form)
		  #'(lambda (term index)
		      (if (and (candidate? index)
			       (static-val? term env))
			  (let ((tv (gentemp)))
			    (setf before `(,tv <- (& ,term)))
			    `(* ,tv))))
		  1))
      (when before
	(return-from extract `(let ,before ,form))))
    ;; Now look for variables and make sure they are in the environment.
    ;; If not, add them, but don't remove them afterwards.  This lets
    ;; user refrain from explicitly adding variables although he will
    ;; still have to get rid of them manually.
    (let ((index 1))
      (dolist (term (cdr form))
	(when (and (dynamic-var? term env)
		   (candidate? index)
		   (not (defined-in-env? term env)))
	  (return-from extract
	    `((add-to-env ,term)
	      ,form)))
	(incf index)))
    ;; Next, get any mentioned variables into registers.
    (let ((index 1))
      (dolist (term (cdr form))
	(when (and (dynamic-var? term env)
		   (candidate? index)
		   (not (in-register? term env)))
	  (return-from extract
	    `((get-in-register ,term)
	      ,form)))
	(incf index)))
    ;; Finally, replace variables with the registers they're in.
    (let (found?)
      (setf (cdr form)
	    (repl2 (cdr form)
		  #'(lambda (term index)
		      (when (and (dynamic-var? term env)
				 (candidate? index)
				 (in-register? term env))
			(true! found?)
			(location term env)))
		  1))
      (if found? form))))		;End function EXTRACT.

