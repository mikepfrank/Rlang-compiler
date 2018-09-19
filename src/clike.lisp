;;; -*- Package: user -*-
(in-package "USER")

;;;----------------------------------------------------------------------
;;; C-like assignment statements.

(definfix (var ++) :opposite --
  `(,var += 1))

(definfix (var --) :opposite ++
  `(,var -= 1))

;; "-" statement: negate the given lvalue in place.
(defconstruct - (var)
  (cond
   ((register? var)
    `(neg ,var))
   (t
    (extract form env :lvalues 1))))

(definfix (var <=< val)
  (cond
   ((and (numberp val) (zerop val))
    '())
   ((and (register? var) (static-val-addr? val env))
    `(rl ,var ,(second val)))
   ((and (register? var) (literal? val env))
    `(rl ,var ,val))
   ((and (register? var) (register? val))
    `(rlv ,var ,val))
   (t
    (extract form env
	     :relevant-terms (if (not (literal? val env)) '(2))
	     :lvalues 1))))

(definfix (var >=> val)
  (cond
   ((and (numberp val) (zerop val))
    '())
   ((and (register? var) (static-val-addr? val env))
    `(rr ,var ,(second val)))
   ((and (register? var) (literal? val env))
    `(rr ,var ,val))
   ((and (register? var) (register? val))
    `(rrv ,var ,val))
   (t
    (extract form env
	     :relevant-terms (if (not (literal? val env)) '(2))
	     :lvalues 1))))

(definfix (var += val) :opposite -=
  (cond
   ((and (numberp val) (zerop val))
    '()) ;Optimization: don't add 0
   ((and (register? var) (static-val-addr? val env))
    `(addi ,var ,(second val)))
   ((and (register? var) (literal? val env))
    `(addi ,var ,val))
   ((and (register? var) (register? val))
    `(add ,var ,val))
   (t
    (extract form env
	     :relevant-terms (if (not (literal? val env)) '(2))
	     :lvalues 1))))
    
(definfix (var -= val) :opposite +=
  (cond
   ((and (register? var) (numberp val))
    `(,var += ,(- val)))		;No SUBI instruction.
   ((and (register? var) (static-array? val env))
    `(,var += ,(intern (concatenate 'string "-" (symbol-name val)))))
   ((and (register? var) (static-val-addr? val env))
    `(addi ,var ,(intern (concatenate 'string "-"
				      (symbol-name (second val))))))
   ((and (register? var)
	 (literal? val env)
	 (negated-sym? val))
    `(,var += ,(positive-of val)))
   ((and (register? var) (register? val))
    `(sub ,var ,val))
   (t
    (extract form env
	     :relevant-terms (if (not (literal? val env)) '(2))
	     :lvalues 1))))

;; Very much like +=.  More abstraction?
(definfix (var ^= val)
  (cond
   ((and (numberp val) (zerop val))
    '()) ;Optimization: don't add 0
   ((and (register? var) (numberp val))
    `(xori ,var ,val))
   ((and (register? var) (register? val))
    `(xor ,var ,val))
   (t
    (extract form env
	     :relevant-terms (if (not (numberp val)) '(2))
	     :lvalues 1))))

(definfix (dest ^=& src1 src2)
  (cond
   ((and (register? dest) (register? src1) (register? src2))
    `(andx ,dest ,src1 ,src2))
   (t
    (extract form env :lvalues 1))))

(definfix (dest ^=<< src1 src2)
  (cond
   ((and (register? dest) (register? src1) (register? src2))
    `(sllvx ,dest ,src1 ,src2))
   (t
    (extract form env :lvalues 1))))

;; This is a logical shift right.
(definfix (dest ^=>> src1 src2)
  (cond
   ((and (register? dest) (register? src1) (register? src2))
    `(srlvx ,dest ,src1 ,src2))
   (t
    (extract form env :lvalues 1))))

;; (base _ offset) gets transformed into this, where dest is
;; some temporary register.
(definfix (dest <->_ base offset)
  ;; THIS IS WRONG!  Extracting before expanding runs the danger
  ;; that the variable assignments used during the extraction
  ;; could be invalidated during the body of the WITH, I think 6/3/97
  (or (extract form env :lvalues 1)
      `(with (,base += ,offset)
	  (,dest <->* ,base))))
    
(definfix (dest <-_ base offset) :opposite ->_
  `(,dest <-* (,base + ,offset)))

(definfix (dest ->_ base offset) :opposite <-_
  `(,dest ->* (,base + ,offset)))

(definfix (dest <-* ptr)
  ;; THIS IS WRONG!  Extracting before expanding runs the danger
  ;; that the variable assignments used during the extraction
  ;; could be invalidated during the header of the LET.
  (or
   (extract form env :lvalues 1)
   (let ((tv (gentemp)))
     `(let (,tv <->* ,ptr)
	(,dest <- ,tv)))))
	 
(definfix (dest ->* ptr)
  (or
   (extract form env :lvalues 1)
   (let ((tv (gentemp)))
     `(let (,tv <->* ,ptr)
	(,dest -> ,tv)))))
	 
(definfix (left <->* rightptr)
  (cond
   ((extract form env :lvalues 1))
   ((and (register? left) (register? rightptr))
    `(exch ,left ,rightptr))))

(definfix (var +=* val1 val2) :opposite -=*
  `(call mult ,var ,val1 ,val2))

(definfix (var -=* val1 val2) :opposite +=*
  `(rcall mult ,var ,val1 ,val2))

(definfix (var +=*/ val1 val2) :opposite -=*/
  `(call _smf ,val1 ,val2 ,var)) ;Note dest is last.

(definfix (var -=*/ val1 val2) :opposite +=*/
  `(rcall _smf ,val1 ,val2 ,var))

(definfix (left <-> right)
  (cond
   ((and (listp right) (eq (car right) '*))
    `(,left <->* ,(cadr right)))
   ((and (location? left) (location? right))
    `(swaploc ,left ,right))
   ((and (listp right)
	 (>= (length right) 3)
	 (dbind (base ~ offset) right
	   (if (eq ~ '_)
	       `(,left <->_ ,base ,offset)))))
   ((and (listp left)
	 (>= (length left) 3)
	 (dbind (base ~ offset) left
	   (if (eq ~ '_)
	       `(,right <->_ ,base ,offset)))))
   ((extract form env :lvalues '(1 2)))))

(defun << (a b)
  (ash a b))
