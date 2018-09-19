;;; -*- Package: user -*-
(in-package "USER")

;;;----------------------------------------------------------------------
;;; Subroutine calling support.
;;;----------------------------------------------------------------------

;;;----------------------------------------------------------------------
;;; User-level constructs.

;; Defsub: Implements subroutine entry/return conventions.
(defconstruct defsub (subname arglist &body body)
  (let ((bodyenv (entryenv arglist env)))
  ;; We wrap it in a branch pair so that if we encounter it from the
  ;; outside we jump over it, and if it runs off its end it comes back to
  ;; the beginning.  This latter behavior facilitates calling a subroutine
  ;; with a single switching-branch to its entry/exit point.
  `((twin-us-branch ,(gentemp "_SUBTOP") ,(gentemp "_SUBBOT") 		
      ;; At start and end of body, environment is as according
      ;; to subroutine calling convention.
      (declare-environment ,bodyenv)
      (portal ,subname) ;Entry/exit point.
      ,@body
      (environment ,bodyenv)))))

;;;----------------------------------------------------------------------
;;; Subroutine calling convention support.

;; NOTE: Currently this does not work right for more than 29 arguments
;; (i.e. when some args need to go on the top of the stack instead of
;; in registers!).
(defconstruct call (subname &rest actualargs)
  `(withargs ,actualargs
     (with-stack-top
	(gosub ,subname))))

(defconstruct rcall (subname &rest actualargs)
  `(withargs ,actualargs
     (with-stack-top
	(rgosub ,subname))))

(defconstruct with-stack-top (&body body)
  (let ((offset (top-of-stack env)))
    `(with-sp-adjustment ,offset
	. ,body)))

(defconstruct with-SP-adjustment (amt &body body)
  ;; AMT must be a literal number.
  `(((reg 1) += ,amt)
    ,@body
    ((reg 1) -= ,amt)))

;; Call a subroutine at a low level with no mention of arguments.
(defconstruct gosub (subname) :opposite rgosub
  ;; A switching branch to the SWAPBRN in the portal should do the trick.
  `(bra ,subname))

(defconstruct rgosub (subname) :opposite gosub
  `(rbra ,subname))

;; Portal: Entry/exit point of a subroutine.
(defconstruct portal (label)
  `((label ,label)
    ;; We always use register $2 for storing our subroutine offsets, by
    ;; convention.
    (swapbr (reg 2))  ;retvar<->BR
    (neg (reg 2))     ;retvar = -retvar
    ))

;;; WITHARGS below needs some work.  It currently can only prepare the 29
;;; arguments that we can fit into registers.  I was intending that if
;;; there are more arguments they should be passed on the stack.  This is
;;; not too hard, but I'm not sure it's worth it.

;; Prepare arguments in conventional locations as for a subroutine call.
(defconstruct withargs (actualargs &body body)
  (let ((result `((vacate (reg 2))
		  . ,body))
	(r 1))
    (dolist (a actualargs)
      (if (and (symbolp a) (not (static-id? a env)))
	  (if (defined-in-env? a env)
	      (push `(relocate ,a ,(argno-to-location r)) result)
	    (setf result
		  `((new-var-at ,a ,(argno-to-location r))
		    ,@result
		    (remove-var ,a))))
	(setf result
	      (let ((tv (gentemp)))
		`((new-var-at ,tv ,(argno-to-location r))
		  ;; This prevents evaluating A from causing
		  ;; earlier-placed registers to change their locations.
		  ;; 6/3/97- But ENSURE-GREEN really enforces more than
		  ;; just this, unfortunately.
		  (ensure-green
		   (,tv <- ,a))
		  ,@result
		  (,tv -> ,a)
		  ;;^-DANGER! Assumes subroutine didn't change value of A.
		  (remove-var ,tv)))))
      (incf r))
    result))
		       
;; On subroutine entry/exit, the environment contains:
;; A return-address variable located in register 2.
;; All the arguments in registers 3,4,... until we run out,
;; and then stack locations -1,-2,... (below top of stack).
;; If not all the registers were used for arguments, then
;; there is a variable for each unused one (above 2), used
;; to ensure that all these other registers are restored to
;; their original state upon exit.
(defun entryenv (arglist origenv)
  (let (locmap (r 0))
    (dolist (a (cons '_RET arglist))
      (push
       `(,a . ,(argno-to-location r))
       locmap)
      (incf r))
    (setf r (+ r 2))
    (if (<= r 31)
	(loop
	 (push `(,(intern (concatenate 'string "_R" (princ-to-string r)))
		 reg ,r) locmap)
	 (incf r)
	 (if (> r 31) (return))))
    (setf locmap (reverse locmap))
    (let ((env (copy-environment origenv)))
      (labels
	  ((is-arg? (name) (member name arglist)))
	(setf (locmap env) locmap
	      (staticvals env) (remove-if #'is-arg? (staticvals env))
	      (staticarrays env) (remove-if #'is-arg? (staticarrays env))))
      env)))

;; Convert an argument number (0 and up) to a location (reg <regno>)
;; or (stack <offset>).  Argument 0 is the return address.
(defun argno-to-location (argno)
  (if (<= argno 29)
      `(reg ,(+ argno 2))
    `(stack ,(- 29 argno))))

