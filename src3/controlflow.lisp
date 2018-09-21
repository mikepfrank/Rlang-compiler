;;; -*- Package: user -*-
(in-package "USER")

;;; ----------------------------------------------------------------------
;;; High-level control flow constructs suitable for user use.

;; if CONDEXPR then
;;    BODY...
;; [else BODY2...]
;;
;; CONDEXPR is evaluated; if result is nonzero body is executed.  In either
;; case, CONDEXPR is then evaluated in reverse.  The value should be the
;; same whether or not BODY was executed, or else behavior undefined.

;; NOTE 6/26/97: IF and all its subsidiary branching constructs need to be
;; completely cleaned up and reorganized.  One big thing is that code for
;; computing EXPR in a condition expression of a form like (EXPR > 0) needs
;; to be wrapped around the entire IF.  And things like (EXPR1 > EXPR2)
;; need to be transformed into ((EXPR1 - EXPR2) > 0).


#| old version of IF
(defconstruct if (condexpr then &body body)
  ;; If the user forgets the word THEN, be nice to him, and figure out what he meant.
  (if (not (eq then 'then))
      (setq body (cons then body)))

  (cond
   ;; If there's an "else" clause, dispatch to the lower-level "ifelse" construct.

   ((member 'else body)
    (let ((ifpart (subseq body 0 (position 'else body)))
	  (elsepart (subseq body (1+ (position 'else body)))))
      `(ifelse ,condexpr ,ifpart ,elsepart)))

   ;; If the condition is a relational expression, dispatch to the
   ;; lower-level "_if" construct which assumes this.

   ((and (listp condexpr)
	 (null (cdddr condexpr))
	 (member (second condexpr) '(= != > <= < >=)))
    `(_if ,condexpr then . ,body))

   ;; For any other kinds of expressions, we don't know any shortcuts; just
   ;; see if the expression evaluates to nonzero.
   (t
    ;; The current version is appropriate only for testing an arbitrary
    ;; value to see if it is non-zero.  For other kinds of conditions,
    ;; other implementations would be more appropriate.
    (if (symbolp condexpr)
	`(_if (,condexpr != 0) then . ,body)
      (let ((tv (gentemp)))
	`(let (,tv <- ,condexpr)
	   (_if (,tv != 0) then . ,body)))))))
|#

;;; New version of IF
(defconstruct if (condexpr then &body body)
  
  (let (thenpart elsepart)
    
    ;; extract then and else parts from body
    (cond ((member 'else body)
           (setq thenpart (subseq body 0 (position 'else body)))
           (setq elsepart (subseq body (1+ (position 'else body)))) )
          
          (t
           (setq thenpart body)
           (setq elsepart nil) ))
    
    ;; break down the conditional expression
    (cond
          ;; If expression is literal zero, execute the ELSE part only (if any).
          ((and (= (length condexpr) 1)
               (numberp (first condexpr))
               (zerop (first condexpr)))
           elsepart)

          ;; If expression is literal number (other than zero), execute THEN part only.
          ((and (= (length condexpr) 1)
                (numberp (first condexpr)))
           thenpart)

          ;; If expression is single variable, compare it to zero.
          ((and (= (length condexpr) 1)
                (variable? (car condexpr)))
           `(if (,(car condexpr) != 0) then . ,body) )

          ;; If expression is logical negation, test for equality with zero.
          ((ifmatch condexpr (! expr)
           `(if (,expr = 0) then . ,body) ))

          ;; Handle binary operations and relations:
          ((ifmatch condexpr (exp1 ~ exp2)
             
           (cond
                 ;; Shortcut AND expression by nesting IFs.
                 ((eq ~ '&&)
                  `(if ,exp1 then (if ,exp2 then . ,body)) )

                 ;; Shortcutting OR efficiently is a little more tricky.
                 ;; NOTE: This method is not very efficient. If the expression
                 ;; on the left of the OR is true, the expression on the right is
                 ;; not evaluated, which maintains the proper semantics. However,
                 ;; the code in the then part is reproduced in the output twice.
                 ((eq ~ '||)
                  `(if ,exp1 then
                     ,thenpart
                   else 
                     (if ,exp2 then
                       ,thenpart
                     else
                       ,elsepart
                     )
                   )
                  )
                  
                  #| other attempts at implementing OR that don't quite work
                  `(let (tmpif <- 0)
                     (if ,exp1 then
                       (tmpif += 1)
                     else
                       (if ,exp2 then
                         (tmpif += 1) )
                     )
                     (if (tmpif) then
                       ,thenpart
                     else
                       ,elsepart
                     )
                     (if (tmpif) then
                       (tmpif -= 1) )
                   )
                  )
                  
                  (let ((bi (gentemp "_IF_OR_BIN"))
                        (bo (gentemp "_IF_OR_BOUT")) )
                    `((if ,exp1 then
                        (go to ,bi from ,bo))
                      (if ,exp2 then
                        (if ,exp1 then
                          (come from ,bo to ,bi))
                        ,.body)) ))
                     
                    (tv (gentemp))
                    (ai (gentemp "_IF_OR_AFTER_IN"))
                    (ao (gentemp "_IF_OR_AFTER_OUT"))
                    `(with (,tv <- ,exp1)
                       (if ,tv then (go to ,bi from ,bo))
                       (if ,exp2 then
                         (if ,tv then (come from ,bo to ,bi))
                         ,.body
                         (if ,tv then (go to ,ao from ,ai)) )
                       (if ,tv then (come from ,ai to ,ao)) )))
                    |#
            
                    
                 ;; Relation where second expression is not literal zero,
                 ;; rearrage expression to create comparison with zero.
                 ((and (member ~ '(>= <= > <))
                       (not (and (numberp exp2)
                                 (zerop exp2) )))
                  `(if ((,exp1 - ,exp2) ,~ 0) then . ,body) )
     
                 ;; Relational or equality operation can't be broken down further,
                 ;; transform to middle-level contructs
                 ((member ~ '(= != >= <= > <))
                  (if (null elsepart)
                    `(_if ,condexpr then . ,body)
                    `(_ifelse ,condexpr ,thenpart ,elsepart) ))
     
                 ;(t `(if (,exp1 != 0) then . ,body))
          )))
    )
  )
)


;; for VAR = START to END
;;    BODY...
;;
;; Semantics: START and END are expressions.  They are each evaluated once
;; forwards at the beginning of the loop, and once in reverse at the end of
;; the loop.  They should return the same value both times.
;;
;; VAR is a fresh variable whose scope is the BODY.  It is set to START,
;; and then the BODY is executed.  If VAR is ever END after executing the
;; body, then the construct immediately terminates.  Otherwise, VAR is
;; incremented by 1 and the BODY is executed again.  START may be equal to
;; END, in which case the BODY is executed exactly once.  If the values of
;; START and END ever change during evaluation, or if BODY ever sets VAR to
;; START-1, the behavior of the entire program becomes undefined.

(defconstruct for (var = start wordto end &body body)
  (let ((top (gentemp "_FORTOP"))	;Loop entry point.
	(bot (gentemp "_FORBOT"))	;Bottom of loop.
	(stvar (gentemp "_FORSTART"))
	(endvar (gentemp "_FOREND")))	;Loop boundary values.
    `(let (,stvar <- ,start)
	  (let (,endvar <- (,end + 1))
	       (scope ,var
		      (,var <- ,stvar)
		      ;; The loop itself.
		      (bcs-branch-pair ,top (,var != ,stvar)
				       ,bot (,var != ,endvar)
			,@body
		        (,var ++))
		      (,var -> ,endvar))))))

;; GOTO - Compute an address and go to it.  The destination must
;; contain appropriate instructions to accept this transfer of control.
;; If the optional LABEL is provided, it provides a name for the
;; origin for the destination instruction to refer to.

(defconstruct goto (expr &optional from label)
  (cond
   ((register? expr)
    (if label
	`((label ,label)
	  (swapbr ,expr))
      `(swapbr ,expr)))
   (t
    (extract form env))))

(defconstruct comefrom (expr &optional to label)
  (cond
   ((register? expr)
    (if label
	`((label ,label)
	  (swapbr ,expr))
      `(swapbr ,expr)))
   (t
    (extract form env))))

(defconstruct go (to label1 from label2)
  `(sbra ,label2 ,label1))

(defconstruct come (from label1 to label2)
  `(sbra ,label2 ,label1))

;;;----------------------------------------------------------------------
;;; Medium-level control flow constructs not intended
;;; for direct user use.

;; InfLoop: Unconditional branch from bottom of body back to top.
;; Also, if we hit it from outside we jump over it.
(defconstruct infloop (&body body)
  `(twin-us-branch ,(gentemp "_LOOPTOP") ,(gentemp "_LOOPBOT")
		   . ,body))

;; ifelse: Lower-level form of (if CONDEXPR then BODY1 else BODY2) that
;; lacks the nice infix notation.

(defconstruct ifelse (condexpr ifstuff elsestuff)
  (cond
   ((and (listp condexpr)
	 (null (cdddr condexpr))
	 (member (second condexpr) '(= != > <= < >=)))
    `(_ifelse ,condexpr ,ifstuff ,elsestuff)))
  )
      
;; Table of opposites of relational symbols.
(setf (get '=  'opposite) '!=
      (get '!= 'opposite) '=
      (get '>  'opposite) '<=
      (get '<= 'opposite) '>
      (get '<  'opposite) '>=
      (get '>= 'opposite) '<
      )

;;; _if (VAR ~ VAL) then BODY
(defconstruct _if ((exp1 ~ exp2) then &body body)
  (let ((l1 (gentemp "_IFTOP"))
        (l2 (gentemp "_IFBOT")))
    `(twin-bcs-branch (,exp1 ,(opposite ~) ,exp2) ,l1 ,l2
	;; The body sure better not change whether the relation holds.
	,@body)))

(defconstruct _ifelse ((exp1 ~ exp2) ifstuff elsestuff)
  (let ((iftop (gentemp "_IFTOP"))
        (ifbot (gentemp "_IFBOT"))
        (elsetop (gentemp "_ELSETOP"))
        (elsebot (gentemp "_ELSEBOT")))
    #|
    `((bcs-branch (,exp1 ,(opposite ~) ,exp2) ,iftop ,elsetop)
      (ensure-green . ,ifstuff)
      (sbra ,ifbot ,elsebot)
      (sbra ,elsetop ,iftop)
      (ensure-green . ,elsestuff)
      (bcs-branch (,exp1 ,~ ,exp2) ,elsebot ,ifbot))
    |#
    `((twin-bcs-branch (,exp1 ,(opposite ~) ,exp2) ,iftop ,ifbot
       ,.ifstuff)
      (twin-bcs-branch (,exp1 ,~ ,exp2) ,elsetop ,elsebot
       ,.elsestuff))
  )
)
      
