;;; -*- Package: user -*-
(in-package "USER")
;;; ----------------------------------------------------------------------
;;; Constructs for variable-environment manipulation (creating/destroying
;;; variables, changing their locations, etc.)
;;; ----------------------------------------------------------------------

;;;----------------------------------------------------------------------
;;; User-level constructs.

;; Create a new variable VAR, bind it to VAL, execute the BODY, and then
;; unbind it from VAL and get rid of it.  VAL may be an expression, but it
;; must evaluate to whatever value VAR actually has at the end of the BODY,
;; or else all bets are off!
;;
;; 6/3/97 - Now LET is slightly more general---VAR can be put into a
;; relationship with VAL in any of a number of ways... <-, <->, ^=, +=...
;; <-, ^=, += are all equivalent given that SCOPE forces VAR to initially
;; be zero, but <-> is different... It sets VAR by swapping it with VAL,
;; which obviously must be a location of some sort.  Afterwards VAR is
;; restored to zero by swapping it back.  Note that in this case, if VAR is
;; not left at zero by the BODY, this is fine and results in VAL being
;; side-effected.  In other words, this kind of LET is effectively
;; temporarily giving VAL a new name which pulls it into a register if say
;; it was originally an array entry.  Another kind of operation (not yet
;; defined) would have VAL be a variable and assign VAR to be truly a
;; synonym for that exact same variable.

(defconstruct let ((var ~ val) &body body)
  (if (eq ~ '<-)
      `(scope ,var
	  (_with (,var <- ,val)
		 . ,body))
    `(scope ,var
	(with (,var ,~ ,val)
	      . ,body))))

;; Declare some vars that should be allocated register locations as soon
;; as they are created.
(defconstruct with-regvars (var-or-vars &body body)
  `(scope ,var-or-vars
     (register ,var-or-vars)
     . ,body))

;; User-level hint to compiler: put the following variables in registers
;; now rather than later.
(defconstruct register (var-or-vars)
  (let ((varlist (if (listp var-or-vars) var-or-vars (list var-or-vars))))
    (mapcar #'(lambda (var) `(get-in-register ,var)) varlist)))

;;;----------------------------------------------------------------------
;;; Intermediate-level constructs.  Not recommended for casual users.

(defconstruct with-location-map (locmapdesc &body body)
  `((locmap ,locmapdesc)
    ,@body
    (locmap ,locmapdesc)))

;; WITH-ENVIRONMENT envdesc body - Ensure that the environment, as far as
;; location maps go, is equivalent to the one specified by environment
;; description ENVDESC, both at the beginning and at the end of the body.
(defconstruct with-environment (envdesc &body body)
  `((environment ,envdesc)
    ,@body
    (environment ,envdesc)))

;; ENVIRONMENT envdesc - Ensure that the environment is equivalent to the
;; one specified by environment description ENVDESC.  ENVDESC must describe
;; an environment object.  Currently the only supported kind of description
;; is an environment object itself.  Environments are equivalent if they
;; have the same variables in the same locations.  ENVIRONMENT will move
;; variables around as necessary to make the environments match, but it
;; will not create or destroy any variables.  If the environments cannot be
;; made to match, currently a compiler error is generated.
(defconstruct environment (envdesc)
  (if (equal-env? env envdesc)
      (values '() envdesc)  ;Tell RCOMP the requested form of the description.
    (if (equal-vars? env envdesc)	;Do the envs have the same variables?
	;; Relocate the first mis-located variable, and try again.
	(let ((v (first-misloc env envdesc)))
	  (if (null (location v envdesc))
	      `(environment ,(set-loc v (location v env) envdesc))
	    `((relocate ,v ,(location v envdesc))
	      (environment ,envdesc))))
      (error "Environments ~s and ~s don't match." env envdesc))))

(defconstruct locmap (locmapdesc)
  (setf env2 (copy-environment env)
	(locmap env2) locmapdesc)
  `(environment ,env2))

(defconstruct declare-locmap (locmapdesc)
  (setf env (copy-environment env)
	(locmap env) locmapdesc)
  (values nil env))

(defconstruct declare-environment (envdesc)
  (setf env (copy-environment envdesc))
  (values nil env))

;; The given variable should exist throughout the body of the scope, no
;; more, no less.  The body must leave the variable clear, or else!
(defconstruct scope (var &body body)
  (let ((vlist (if (listp var) var (list var))))
    `(with ,(mapcar #'(lambda (var) `(add-to-env ,var)) vlist)
	   ,@body))
  ;; Note danger if var is not actually clear at end of BODY!
  )

;; ENSURE-GREEN enforces environmental correctness -- it leaves the
;; environment just the way it found it. (With regards to its location map.)
(defconstruct ensure-green (&body body)
  `(,@body
    (locmap ,(locmap env))))

;; DECLARE-GREEN declares that the environment in effect when the body
;; is entered will necessarily be in effect when it ends.  Don't use this
;; when it isn't true!
(defconstruct declare-green (&body body)
  (values body env))

;; Make the LOCATION be clear, and associate it with a new variable VARNAME.
(defconstruct new-var-at (varname location)
  (if (defined-in-env? varname env)
      (error "Variable ~s is already in the environment!" varname)
    `((vacate ,location)
      (tell-loc ,varname ,location))))

;; Make a particular location LOC become available (empty, and no variable
;; assigned to it).
(defconstruct vacate (loc)
  (let ((v (var-at-loc loc env)))
    (if v
	;; Location is occupied by variable V.
	(let ((reg (next-avail-reg env)))
	  ;; If any registers are available, move V there.
	  (if reg `(relocate ,v ,reg)
	    ;; Else move V to the next available stack location.
	    (let ((s (next-avail-stack env)))
	      (if s `(relocate ,v ,s))))))))

;; Arrange for the given variable, which should already be present in the
;; environment, to be located in a register (instead of on the stack).
(defconstruct get-in-register (var)
  (if (symbolp var)
      (let ((l (location var env)))
	(if (not (register? l))		;If it's not already in a register,
	    (let ((reg (next-avail-reg env)))
	      (if reg	    ;If there is a register avaiable, put it there.
		  (if (null-loc? l)
		      `(tell-loc ,var ,reg)
		    `(relocate ,var ,reg))
		;; Else boot out the least-recently moved variable.
		(let* ((victim (least-recently-moved env))
		       (loc (location victim env)))
		  (if (null-loc? l)
		      `((vacate ,loc)
			(tell-loc ,var ,loc))
		    `(relocate ,var ,loc)))))))))

;;;----------------------------------------------------------------------
;;; Primitive environment-manipulation constructs.

;;::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
;; ADD-TO-ENV: Create the given variable in the environment, but don't
;; give it a location quite yet.
;;
;; Semantics amendment, 8/4/01 - Actually, not giving every new
;; variable a location right away turns out be dangerous, because the
;; first manipulation of the variable might be within an ensure-green
;; context, so the variable's location will be forgotten.  (Actually,
;; in the present version this elicits an infinite loop.)  So, we're
;; changing the semantics of ADD-TO-ENV to go ahead and assign the
;; variable to a register.
;;::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
(defconstruct add-to-env (var) :opposite remove-var
  `((_add-to-env ,var)
    (get-in-register ,var)))

;; _ADD-TO-ENV - Like ADD-TO-ENV, but really doesn't give the
;; variable a location yet.  Don't ever use this except the way
;; it's used in ADD-TO-ENV!!

(defconstruct _add-to-env (var)
  (if (defined-in-env? var env)
      (error "Variable ~s already exists!" var)
    (values '() (set-loc var nil env))))

;; Change the current environment to have the location of variable VAR as
;; being LOC.  Generates no code.  This construct is dangerous if the old
;; location of VAR has a non-zero runtime value, and is not associated with
;; any other variable.
(defconstruct tell-loc (var loc)
  (values '() (set-loc var loc env)))

;; Assuming that a variable is empty, remove it from the environment!
;; (Danger, Will Robinson!)  This causes grave problems if the runtime
;; value of the variable is not zero.  But currently we generate no runtime
;; code to notice that condition, so watch out!
(defconstruct remove-var (varname) :opposite add-to-env
  (values '() (remove-var varname env)))

;;;----------------------------------------------------------------------
;;; Low-level utilities for working with variables.

(defun variable? (form) (symbolp form))
