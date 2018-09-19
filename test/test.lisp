;;; -*- Package: user -*-
(in-package "USER")
;;;----------------------------------------------------------------------
;;; Testing code.

(defun test-sch ()
  (rcompile-file "../test/sch/sch.r"))

(defun test-sch-debug ()
  (rcompile-file "../test/sch/sch.r" :debug t))

(defparameter *test*
  '(defprog example1
     ()
     (let p = (3 * 5))))

(defparameter *test2*
  '(defprog example-program-2 ()
     (call mult 3 5 p)))

(defun test ()
  (rc *test*))

;; My original very simple MULT routine.
(defparameter *mult-orig*
  '(defsub mult (m1 m2 prod)
      ;; Use grade-school algorithm.
      (for pos = 0 to 31	       ; For each of the 32 bit-positions,
	 (if (m1 & (1 << pos)) then    ;   if that bit of m1 is 1, then
	    (prod += (m2 << pos))))))  ;     add m2, shifted over to that
				       ;       position, into p.

;; I'd like this code to compile to produce exactly my hand-compiled-
;; and -optimized version of the MULT routine.  Currently, it won't though.
(defparameter *mult-opt*
  '(defsub mult-opt (m1 m2 product)
     (with-register-vars ((limit = 32) (mask = 1) shifted bit position)
	(for position = 0 until limit
	   (with (bit <- (m1 & mask))
	      (if bit then
	         (with (shifted <- (m2 << position))
		    (product += shifted))))
	   (mask <=< 1))))
  )

;; Hand-compiled, optimized multiply routine.  We could actually
;; include this in programs if we want.
(defparameter *mult-hand*
  '(alloctop
    (bra   allocbot)
    alloc4
    (swbrn $2)	   ; This sub-subroutine frees
    (addi  $1  +1) ; 4 registers for use in the
    (exch  $31 $1) ; MULT subroutine.  It leaves
    (addi  $1  +1) ; the stack pointer pushed
    (exch  $30 $1) ; above, but we don't mind.
    (addi  $1  +1)
    (exch  $29 $1)
    (addi  $1  +1)
    (exch  $28 $1)
    allocbot
    (bra   alloctop)
    ;; This subroutine's arguments are in registers $3, $4, and $5.
    subtop
    (bra   subbot)		; MULT top.
    mult
    (swbrn $2)  		; Subroutine entry/exit point.
    (exch  $2 $1)               ; Push return address.
    (bra   alloc4)		; Allocate 4 registers ($28-$31).
    (addi  $31 32)		; limit <- 32
    (addi  $2 1)		; mask <- 1
    fortop
    (bne   $30 $0 forbot)       ; unless (position != 0) do
    (andx  $28 $3 $2)	        ;     bit <- m1&mask
    iftop
    (beq   $28 $0 ifbot)	;     if (bit != 0) then
    (sllvx $29 $4 $30)  	;        shifted <- m2<<position
    (add   $5 $29)		;	 product += shifted
    (sllvx $29 $4 $30)  	;	 shifted -> m2<<position
    ifbot
    (beq   $28 $0 iftop)	;     end if
    (andx  $28 $3 $2)   	;     bit -> m1&mask
    (rl    $2 1)		;     mask <=< 1 (rotate left by 1)
    (addi  $30 +1)		;     i++
    forbot
    (bne   $30 $31 fortop)	; and repeat while (position != limit).
    (sub   $30 $31)		; position -> limit
    (addi  $2 -1)		; mask -> 1
    (addi  $31 -32)		; limit -> 32
    (rbra  alloc4)		; Deallocate 4 registers ($28-$31).
    (exch  $2 $1)		; Pop return address.
    subbot
    (bra   subtop)		; MULT bottom.
    ))

(defparameter *mult-frac*
  ;; Like mult, but interprets the multiplier (1st arg) to be a
  ;; number between 1 and -1, on a scale where 2^31 = 1, -2^31 = -1.
  '(defsub mult-frac (m1 m2 prod)
      (for pos = 0 to 31
	 (if (m1 & (1 << pos)) then
	    (prod += (m2 >> (31 - pos)))))))

;;; Interesting question: does MULT-FRAC yield the same result
;;; independently of the order of m1 and m2?  I know the original
;;; MULT did.  I think it does.

;; This optimized version the same number of instructions as mult-opt.
;; Put this in the standard library?
(defparameter *mult-frac-opt*
  '(defsub mult-frac-opt (m1 m2 product)
     (with-registers ((limit = 32) (mask = 1) shifted bit position)
	(for position = 0 until limit
	   (mask >=> 1)
	   (with (bit <- (m1 & mask))
	      (if bit then
	         (with (shifted <- (m2 >> position))
		    (product += shifted))))
	   )))
  )

;;; Now, what to do if integers are signed?
;;; What to do: for actual multiplication, only look at bits
;;; 30-0 of multiplier.  Branch on bit 31.  If 1, then
;;; subtract shifted multiplicand from product instead of
;;; adding it.

(defparameter *signed-mult-frac-opt*
  '(defsub signed-mult-frac-opt (m1 m2 product)
     (with-registers ((limit = 32) mask shifted bit position)
	(mask <- (1 << 31))
	(for position = 1 until limit
	   (mask >=> 1)
	   (with (bit = (m1 & mask))
	      (if bit then
	         (with (shifted = (m2 >> position))
		    (if (m2 > 0)
			(product += shifted)
		      else
		        (product -= shifted))))))
	(mask -> 1))))


;; This will be the first version of SIGNED-MULT-FRAC to actually
;; be compilable. (NOT. -6/26)
(defparameter *smf-first*
  '(defsub smf-first (m1 m2 prod)
     (with-regvars (mask shifted bit)
       (with (mask <- 1)
	  (mask <=< 31)
	  (for position = 1 to 31
	     (mask >=> 1)		;Rotate right by 1 bit
	     (with (bit <- (m1 & mask))
		(if bit then
		  (with (shifted <- (m2 >> position))
		     (if (m2 > 0)
			 (prod += shifted)
		       else
		         (prod -= shifted))))))))))

;; OK, now THIS version is the new target. 6/26
(defparameter *smf-new*
  '(defsub smf-new (m1 m2 prod)
     (with-regvars (m1p m2p mask shifted bit p)
       (with ((mask <- 1)
	      (m1p <- m1) (if (m1 < 0) then (- m1p))
	      (m2p <- m2) (if (m2 < 0) then (- m2p)))
	  (mask <=< 31)
	  (for position = 1 to 31
	     (mask >=> 1)		;Rotate right by 1 bit
	     (with (bit <- (m1 & mask))
		(if bit then
		  (with (shifted <- (m2 >> position))
		     (p += shifted)))))
	  (if (m1 < 0) then (- p))
	  (if (m2 < 0) then (- p))
	  (prod += p)))))
  
;;; Stuff for Schroedinger program.
;; Point function.
(defparameter *pfunc*
  '(defsub pfunc (dest src i alphas epsilon)
      ((dest _ i) += (((alphas _ i) << 1) */ (src _ i)))
      ((dest _ i) -= (epsilon */ (src _ ((i + 1) & 127))))
      ((dest _ i) -= (epsilon */ (src _ ((i - 1) & 127)))))
  )

;; Wavefunction step.
(defparameter *schstep*
  '(defsub schstep (psiR psiI alphas epsilon)
      (for i = 0 until 128		;For each point i,
	 (call pfunc psiR psiI i))	; psiR[i] += func(psiI,i)
      (for i = 0 until 128		;For each point i,
	 (rcall pfunc psiI psiR i)))    ; psiI[i] -= func(psiR,i)
  )

;; Data for the schroedinger simulation:
;; Epsilon, alphas, and psis.
(defparameter *schdata*
  '((array epsilon 203667001)
    (array alphas
	   458243442 456664951 455111319 453582544 452078627 450599569
	   449145369 447716027 446311542 444931917 443577149 442247239
	   440942188 439661994 438406659 437176182 435970563 434789802
	   433633899 432502854 431396668 430315339 429258869 428227257
	   427220503 426238607 425281569 424349389 423442068 422559605
	   421701999 420869252 420061363 419278332 418520159 417786845
	   417078388 416394790 415736049 415102167 414493143 413908977
	   413349669 412815220 412305628 411820895 411361019 410926002
	   410515843 410130542 409770099 409434515 409123788 408837920
	   408576909 408340757 408129463 407943027 407781450 407644730
	   407532868 407445865 407383720 407346432 407334003 407346432
	   407383720 407445865 407532868 407644730 407781450 407943027
	   408129463 408340757 408576909 408837920 409123788 409434515
	   409770099 410130542 410515843 410926002 411361019 411820895
	   412305628 412815220 413349669 413908977 414493143 415102167
	   415736049 416394790 417078388 417786845 418520159 419278332
	   420061363 420869252 421701999 422559605 423442068 424349389
	   425281569 426238607 427220503 428227257 429258869 430315339
	   431396668 432502854 433633899 434789802 435970563 437176182
	   438406659 439661994 440942188 442247239 443577149 444931917
	   446311542 447716027 449145369 450599569 452078627 453582544
	   455111319 456664951)
    ;; This is the shape of the initial wavefunction.
    (array psis
	   2072809 3044772 4418237 6333469 8968770 12546502 17338479
	   23669980 31921503 42527251 55969298 72766411 93456735 118573819
	   148615999 184009768 225068513 271948808 324607187 382760978
	   445857149 513053161 583213481 654924586 726530060 796185813
	   861933650 921789572 973841548 1016350163 1047844835 1067208183
	   1073741824 1067208183 1047844835 1016350163 973841548 921789572
	   861933650 796185813 726530060 654924586 583213481 513053161
	   445857149 382760978 324607187 271948808 225068513 184009768
	   148615999 118573819 93456735 72766411 55969298 42527251 31921503
	   23669980 17338479 12546502 8968770 6333469 4418237 3044772
	   2072809 1393998 926112 607804 394060 252382 159681 99804 61622
	   37586 22647 13480 7926 4604 2642 1497 838 463 253 136 73 38 20
	   10 5 2 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
	   0 0 0 0 0 0 0 0 0))
  )


(defparameter *sch*
  '((DEFWORD EPSILON 203667001)
    (DEFARRAY ALPHAS 458243442 456664951 455111319 453582544
      452078627 450599569 449145369 447716027 446311542 444931917
      443577149 442247239 440942188 439661994 438406659 437176182
      435970563 434789802 433633899 432502854 431396668 430315339
      429258869 428227257 427220503 426238607 425281569 424349389
      423442068 422559605 421701999 420869252 420061363 419278332
      418520159 417786845 417078388 416394790 415736049 415102167
      414493143 413908977 413349669 412815220 412305628 411820895
      411361019 410926002 410515843 410130542 409770099 409434515
      409123788 408837920 408576909 408340757 408129463 407943027
      407781450 407644730 407532868 407445865 407383720 407346432
      407334003 407346432 407383720 407445865 407532868 407644730
      407781450 407943027 408129463 408340757 408576909 408837920
      409123788 409434515 409770099 410130542 410515843 410926002
      411361019 411820895 412305628 412815220 413349669 413908977
      414493143 415102167 415736049 416394790 417078388 417786845
      418520159 419278332 420061363 420869252 421701999 422559605
      423442068 424349389 425281569 426238607 427220503 428227257
      429258869 430315339 431396668 432502854 433633899 434789802
      435970563 437176182 438406659 439661994 440942188 442247239
      443577149 444931917 446311542 447716027 449145369 450599569
      452078627 453582544 455111319 456664951)
    (DEFARRAY PSIR 2072809 3044772 4418237 6333469 8968770 12546502
      17338479 23669980 31921503 42527251 55969298 72766411 93456735
      118573819 148615999 184009768 225068513 271948808 324607187
      382760978 445857149 513053161 583213481 654924586 726530060
      796185813 861933650 921789572 973841548 1016350163 1047844835
      1067208183 1073741824 1067208183 1047844835 1016350163
      973841548 921789572 861933650 796185813 726530060 654924586
      583213481 513053161 445857149 382760978 324607187 271948808
      225068513 184009768 148615999 118573819 93456735 72766411
      55969298 42527251 31921503 23669980 17338479 12546502 8968770
      6333469 4418237 3044772 2072809 1393998 926112 607804 394060
      252382 159681 99804 61622 37586 22647 13480 7926 4604 2642
      1497 838 463 253 136 73 38 20 10 5 2 1 0 0 0 0 0 0 0 0 0 0 0 0
      0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
    (DEFARRAY PSII 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
      0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
      0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
      0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
      0 0 0 0 0 0 0 0 0 0 0)
    (DEFSUB PFUNC (DEST SRC I ALPHAS EPSILON)
      ((DEST _ I) += ((ALPHAS _ I) */ (SRC _ I)))
      ((DEST _ I) -= (EPSILON */ (SRC _ ((I + 1) & 127))))
      ((DEST _ I) -= (EPSILON */ (SRC _ ((I - 1) & 127)))))
    (DEFSUB SCHSTEP (PSIR PSII ALPHAS EPSILON)
      (FOR I = 0 TO 127 (CALL PFUNC PSIR PSII I))
      (FOR I = 0 TO 127 (RCALL PFUNC PSII PSIR I)))
    (DEFSUB PRINTWAVE (WAVE)
      (FOR I = 0 TO 127 (PRINTWORD (WAVE _ I))) (PRINTLN))
    (DEFMAIN SCHROED
      (FOR I = 1 TO 50
	   (FOR J = 1 TO 20 (CALL SCHSTEP PSIR PSII ALPHAS EPSILON))
	   (CALL PRINTWAVE PSIR) (CALL PRINTWAVE PSII)))))

(defparameter *sch-frag1*
  '((DEFWORD EPSILON 203667001)
    (DEFARRAY ALPHAS 458243442 456664951)
    (DEFARRAY PSIR 2072809 3044772)
    (DEFARRAY PSII 0 0)
    (DEFSUB PFUNC (DEST SRC I ALPHAS EPSILON)
      ((DEST _ I) += ((ALPHAS _ I) */ (SRC _ I)))
      ((DEST _ I) -= (EPSILON */ (SRC _ ((I + 1) & 127))))
      ((DEST _ I) -= (EPSILON */ (SRC _ ((I - 1) & 127)))))
    (DEFSUB SCHSTEP (PSIR PSII ALPHAS EPSILON)
      (FOR I = 0 TO 127 (CALL PFUNC PSIR PSII I))
      (FOR I = 0 TO 127 (RCALL PFUNC PSII PSIR I)))
    (DEFSUB PRINTWAVE (WAVE)
      (FOR I = 0 TO 127 (PRINTWORD (WAVE _ I))) (PRINTLN))
    (DEFMAIN SCHROED
      (FOR I = 1 TO 50
	   (FOR J = 1 TO 20 (CALL SCHSTEP PSIR PSII ALPHAS EPSILON))
	   (CALL PRINTWAVE PSIR) (CALL PRINTWAVE PSII)))))

(defparameter *sch-frag2*
  '((DEFWORD EPSILON 203667001)
    (DEFARRAY ALPHAS 458243442)
    (DEFARRAY PSIR 2072809)
    (DEFARRAY PSII 0)
    (CALL SCHSTEP PSIR PSII ALPHAS EPSILON)))

(defparameter *sch-frag3*
  '((DEFWORD EPSILON 203667001)
    (DEFARRAY ALPHAS 458243442)
    (CALL SCHSTEP ALPHAS EPSILON)))
