;;; -*- Package: user -*-
(in-package "USER")
;;;----------------------------------------------------------------------
;;; Support for various kinds of branches.
;;; These constructs are not intended to appear in source code,
;;; but are rather used to implement higher-level control-flow
;;; constructs.
;;;----------------------------------------------------------------------

;;;----------------------------------------------------------------------
;;; Relatively high-level branch constructs.

;; paired binary conditional switching branches.  The body in between the
;; two branches must conserve the environment or else we won't have a
;; definite compile-time idea of the environment after the branch pair
;; because we don't know whether the branch succeeds or fails at run-time
;; or not.  Note the vars must really be variables and not literals or
;; registers.
(defconstruct bcs-branch-pair (toplab (vara1 ~a vara2)
			       botlab (varb1 ~b varb2)
			       &body body)
  `(;; All the variables involved need to be in registers before we start.
    (get-in-register ,vara1)
    (get-in-register ,vara2)
    (get-in-register ,varb1)
    (get-in-register ,varb2)
    (bcs-branch (,vara1 ,~a ,vara2) ,toplab ,botlab)
    ;; Since the vars were already in registers, BCS-BRANCH will not have
    ;; modified the environment after the branch point.
    (ensure-green ;Complain if the body doesn't clean up after itself. 
     ,@body)
    ;; Due to the above GET-IN-REGISTERs and the ENSURE-GREEN, the b
    ;; variables will already be in registers here, so this BCS-BRANCH will
    ;; not need to modify the environment at all from the current one,
    ;; which is identical to the one just before the branch point.
    (bcs-branch (,varb1 ,~b ,varb2) ,botlab ,toplab)))
    
;; Twin (meaning with identical tests and variables) binary-operator
;; conditional switching branches.  The body in between the two
;; branches must conserve the environment or else we won't have a
;; definite compile-time idea of the environment after the branch pair
;; because we don't know whether the branch succeeds or fails at
;; run-time or not.
(defconstruct twin-bcs-branch ((exp1 ~ exp2) toplab botlab &body body)
  (cond
   ;; Relations that the instruction set supports directly.
   ((member ~ *bc-branch-instructions*)
    `(_twin-bcs-branch ,exp1 ,~ ,exp2 ,toplab ,botlab ,@body))

   ;; Relations we can convert to comparison with zero.
   ((member ~ '(> < >= <= = !=))
    `(_twin-bcs-branch (,exp1 - ,exp2) ,~ 0 ,toplab ,botlab ,@body))
   (t
    (error "TWIN-BCS-BRANCH: Unknown relation ~s.~&" ~))))

;; Like twin-bcs-branch, but with an uglier syntax that allows EXTRACT
;; to be used.
(defconstruct _twin-bcs-branch (exp1 ~ exp2 toplab botlab &body body)
  (cond 
   ((and (register? exp1) (or (register? exp2) (zerop exp2)))
    ;; In this case we don't have to get the variables into registers.
    `((bcs-branch (,exp1 ,~ ,exp2) ,toplab ,botlab)
      ;; bcs-branch had better not itself change the environment
      ;; after it branches!
      (ensure-green
      ,@body)
      (bcs-branch (,exp1 ,~ ,exp2) ,botlab ,toplab)))
   ((and (variable? exp1) (or (variable? exp2) (zerop exp2)))
    `((get-in-register ,exp1)
      (get-in-register ,exp2)
      (bcs-branch (,exp1 ,~ ,exp2) ,toplab ,botlab)
      ;; bcs-branch had better not itself change the environment
      ;; after it branches!
      (ensure-green
       ,@body)
      (bcs-branch (,exp1 ,~ ,exp2) ,botlab ,toplab)))
   (t
    (extract form env
	     :relevant-terms '(1 3)))))

;; Twin unconditional switching branches.
;; Now that the environment contains information other than
;; the locations of run-time variables, does it make
;; sense for it to be completely green?  Declaring static variables
;; inside the body might be expected to be able to affect the
;; outside world.  Or, maybe it shouldn't.  Haven't decided yet.
(defconstruct twin-us-branch (toplab botlab &body body)
  ;; TWIN-US-BRANCH is GREEN on the outside because encountering it from
  ;; the outside you just jump over it, and the environment doesn't change.
  `(declare-green
    (sbra-pair ,toplab ,botlab . ,body))
  ;; We have no idea what the environment is inside the first bra, though
  ;; (cuz we might slip into the middle as a subroutine call), so we can't
  ;; put an ensure-green or anything in there.  The environment in effect
  ;; at this point, which just leeches in from above, is usually wrong.
  ;; Still, the body needs to be careful that whatever environment is in
  ;; effect at its end is the same as the one it assumes at its top.  But
  ;; this is a job for a higher level to worry about.
  )

;;;----------------------------------------------------------------------
;;; lower-level branch constructs.

;;; Note to self: I think that the way branches and environments
;;; interact may be incorrect.  If the variables mentioned in the
;;; branch are not in registers, then the environment needs to change
;;; to allow the branch to be done---so the environment declared
;;; at the place we're branching to may be wrong.  Need to go thru
;;; and fix carefully.  Really, need a more sophisticated approach
;;; to how environments are kept track of during compilation of
;;; complex control-flow structures.

;; This low-level thing doesn't worry about environments at all.
;; That's a job for higher-level dudes that use it.
(defconstruct sbra-pair (toplab botlab &body body)
  `((sbra ,toplab ,botlab)
    ,@body
    (sbra ,botlab ,toplab)))

;; 
;; SBRA: Switching branch (unconditional).  Branch is
;; from label thislab to otherlab.
;;
;; Semantics is: if we branch to this statement from
;; otherlab, then continue forwards normally.  If we
;; arrive at this statement normally, then branch to
;; otherlab.  If we arrive at this statement some other
;; way, results are undefined.
;; 
(defconstruct sbra (thislab otherlab)
  `((label ,thislab)  ;Convention: label precedes labeled statement.
    (bra ,otherlab))) ;Assume that the hardware gives the semantics we want.

(defconstruct sbez (thislab var lab)
  ;; There's no built-in BEZ, so we reserve reg $0 to be zero
  ;; so we can just use BEQ instead.
  `(bcs-branch (,var = (reg 0)) ,thislab ,lab))

(defconstruct sbnz (thislab var lab)
  ;; There's no built-in BNZ, so we reserve reg $0 to be zero
  ;; so we can just use BNE instead.
  `(bcs-branch (,var != (reg 0)) ,thislab ,lab))

(defconstruct bcs-branch ((var1 ~ var2) thislab otherlab)
  (cond
   ;; If the variables aren't in registers, get them there.  The only thing
   ;; is, I'm not sure it makes sense to do any environment manipulation at
   ;; this level because it makes it difficult for the higher level stuff
   ;; to ensure thtat the environment is consistent at both ends of the
   ;; actual branch point.
   ((and (member ~ '(= !=)) (numberp var2) (zerop var2))
    ;; To compare for equality with zero, compare with $0 (reserved for 0).
    `(bcs-branch (,var1 ,~ (reg 0)) ,thislab ,otherlab))
   ((and (or (register? var1)
	     (in-register? var1 env)
	     (eql var1 0))
	 (or (register? var2)
	     (in-register? var2 env)
	     (eql var2 0)))
    `((label ,thislab)
      ;; This assumes that BC-BRANCH has the right switching sort of
      ;; semantics, and that it doesn't insert any instructions before the
      ;; actual branch.
      (bc-branch (,var1 ,~ ,var2) ,otherlab)))
   ;; Does this make sense?
   ((and (symbolp var1) (defined-in-env? var1 env)
	 (not (in-register? var1 env)))
    `((get-in-register ,var1)
      (bcs-branch (,var1 ,~ ,var2) ,thislab ,otherlab)))
   ((and (symbolp var2) (defined-in-env? var2 env)
	 (not (in-register? var2 env)))
    `((get-in-register ,var2)
      (bcs-branch (,var1 ,~ ,var2) ,thislab ,otherlab)))
   ((and (symbolp var1) (not (defined-in-env? var1 env)))
    `((add-to-env ,var1)
      (bcs-branch (,var1 ,~ ,var2) ,thislab ,otherlab)))
   ((and (symbolp var2) (not (defined-in-env? var2 env)))
    `((add-to-env ,var2)
      (bcs-branch (,var1 ,~ ,var2) ,thislab ,otherlab)))
   ;; I could go on and handle expressions and literals as well, but again
   ;; I'm concerned that this isn't the right level.
   ))
   
;; binary conditional branch
(defconstruct bc-branch ((var1 ~ var2) destlab)
  (cond
   ((and (or (register? var1) (eql var1 0))
	 (or (register? var2) (eql var2 0)))
    `(rbc-branch (,var1 ,~ ,var2) ,destlab))
   ;; If the variables are in registers, just look at the registers.
   ((and (symbolp var1) (in-register? var1 env))
    `(bc-branch (,(location var1 env) ,~ ,var2) ,destlab))
   ((and (symbolp var2) (in-register? var2 env))
    `(bc-branch (,var1 ,~ ,(location var2 env)) ,destlab))
   ;; We can't really do any environment manipulation at this level
   ;; because the instructions manipulated will come between us and the
   ;; label inserted by BCS-BRANCH.
   (t
    (error "BC-BRANCH can only cope with variables residing in registers!")
    )))

;;;----------------------------------------------------------------------
;;; Branching primitives.

;; A label is a tag that gives an address in code that is a target
;; for branching.
(defconstruct label (labname)
  labname)

(defconstruct bez (reg lab)
  ;; There's no built-in BEZ, so we reserve reg $0 to be zero
  ;; so we can just use BEQ instead.
  `(rbc-branch (,reg = (reg 0)) ,lab))

(defconstruct bnz (reg lab)
  ;; Similar, use BNE.
  `(rbc-branch (,reg != (reg 0)) ,lab))

;; Binary conditional branch operator symbols.
(defparameter *bc-branch-instructions*
  '((!= . bne) (= . beq)))

(defparameter *zerocmp-branch-instructions*
  '((>= . bgez) (<= . blez) (> . bgtz) (< . bltz)))

;; "register binary conditional branch"
;; tests whether two registers satisfy some relation ~ and if so
;; branch to DESTLAB.
(defconstruct rbc-branch ((reg1 ~ reg2) destlab)
  (cond
   ((assoc ~ *bc-branch-instructions*)
    `(,(cdr (assoc ~ *bc-branch-instructions*))
      ,reg1 ,reg2 ,destlab))
   ((and (eql reg2 0)
	 (assoc ~ *zerocmp-branch-instructions*))
    `(,(cdr (assoc ~ *zerocmp-branch-instructions*))
      ,reg1 ,destlab))
   (t
    ;; No built-in instruction to compare the two registers.
    ;; We have to subtract & compare with zero instead.
    (error "Can't RBC-BRANCH with ~s operator.~&" ~)
    )))
