;;; -*- Package: user -*-
(in-package "USER")

;;;----------------------------------------------------------------------
;;; Register/stack manipulation.

(defconstruct relocate (var loc)
  (let ((oldloc (location var env)))
    (if (not (equal oldloc loc))	;If not already there.
	(if (null oldloc)
	    `((vacate ,loc)
	      (tell-loc ,var ,loc))
	  (if (null loc)
	      `(tell-loc ,var ,loc)
	    (let ((oldv (var-at-loc loc env)))
	      (if oldv
		  ;; If new location occupied, swap.
		  `((swaploc ,oldloc ,loc)
		    (tell-loc ,var ,loc)
		    (tell-loc ,oldv ,oldloc))
		;; Not occupied, just move.
		(if (null-loc? oldloc)
		    `(tell-loc ,var ,loc)
		  `((moveloc ,oldloc ,loc)
		    (tell-loc ,var ,loc))))))))))

;;; loc1 and loc2 should be register or stack locations.
(defconstruct swaploc (loc1 loc2)
  (if (and (register? loc1)
	   (register? loc2))
      `(swapregs ,loc1 ,loc2)
    (if (register? loc1)
	`(exregstack ,loc1 ,loc2)
      (if (register? loc2)
	  `(exregstack ,loc2 ,loc1)
	;; We need a temporary register to facilitate the stack exchange;
	;; we choose reg. 31 for no particular reason.  The net change to
	;; it is nil.  This all works but could probably be made
	;; considerably more efficient.
	`((exregstack (reg 31) ,loc1)
	  (exregstack (reg 31) ,loc2)
	  (exregstack (reg 31) ,loc1))))))

;; Assuming loc2 is clear, move loc1 to it.
(defconstruct moveloc (loc1 loc2)
  (if (and (register? loc1)
	   (register? loc2))
      `(movereg ,loc1 ,loc2)
    (if (register? loc1)
	`(exregstack ,loc1 ,loc2)
      (if (register? loc2)
	  `(exregstack ,loc2 ,loc1)
	;; We need a temporary register to facilitate the stack exchange;
	;; we choose reg. 31 for no particular reason.  The net change to
	;; it is nil.  This all works but could probably be made
	;; considerably more efficient.
	`((exregstack (reg 31) ,loc1)
	  (exregstack (reg 31) ,loc2)
	  (exregstack (reg 31) ,loc1))))))

;; Would save a lot of ADDI instructions if I changed this to modify the
;; stack pointer before but not after; and instead change the environment
;; to reflect correct new variable locations and amount of stack adjustment
;; from original.  But perhaps it would be better to leave the stack
;; pointer alone and get rid of adjacent ADDIs via a later peephole
;; optimization or something.
(defconstruct exregstack (reg stackloc)
  `(with-SP-adjustment ,(offset stackloc)
	(exch ,reg (reg 1))))

;; Given a register, push its contents onto the stack.  Not currently used.
(defconstruct push (reg)
  `((exch ,reg (reg 1)) ;; <-- Convention: $1 is stack pointer.
    (++ ,reg)))

;;;----------------------------------------------------------------------
;;; Pure register manipulation.

;; swapregs R1 R2 - Given two registers, swap their contents.
(defconstruct swapregs (r1 r2)
  ;; Implementation for architectures that support XOR'ing regs, but not
  ;; swapping regs directly.  Another way uses +=, -=, and NEG but takes 4
  ;; instructions.
  `((,r1 ^= ,r2)
    (,r2 ^= ,r1)
    (,r1 ^= ,r2)))

;; Fast way to move r1 to r2 when r2 is known to be empty!  Otherwise
;; behavior is "undefined" (actually in this implementation r1 gets r2, but
;; r2 ends up with r2^r1).
(defconstruct movereg (r1 r2)
  `((,r2 ^= ,r1)
    (,r1 ^= ,r2)))
