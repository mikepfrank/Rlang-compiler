;;; -*- Package: user -*-
(in-package "USER")

(defconstruct standard-library ()
  ;; List of standardly available subroutines.
  '((def-smf)))

;; Define the simplest normal integer multiplication function.
;; This adds the low word of the product of unsigned integers
;; M1 and M2 into PROD.

(defconstruct def-mult ()
  '(defsub mult (m1 m2 prod)
    ;; Use grade-school algorithm.
    (for pos = 0 to 31			; For each of the 32 bit-positions,
       (if (m1 & (1 << pos)) then	;   if that bit of m1 is 1, then
	 (prod += (m2 << pos))))))	;     add m2, shifted over to that
					;       position, into p.

;; Define the signed multiplication-by-fraction function, which takes
;; two signed integers M1 and M2, and adds the high word of their true
;; integer product into PROD.  This is like multiplying M1 by M2 when
;; M2 is considered to represent a fraction with numerator explicit and
;; denominator 2^31.
;;
;; This version of the function was tested in C and seemed to work
;; satisfactorily, although more testing is needed.  Need to compare
;; with upper bits of true (64-bit-wide) product. 6/26/97

(defconstruct def-smf ()
  '(defsub _smf (m1 m2 prod)
     (with-regvars (m1p m2p mask shifted bit p)
       (with ((mask <- 1)
	      (m1p <- m1) (if (m1 < 0) then (- m1p))
	      (m2p <- m2) (if (m2 < 0) then (- m2p)))
	  (mask <=< 31)
	  (for position = 1 to 31
	     (mask >=> 1)
	     (with (bit <- (m1p & mask))
		(if bit then
		  (with (shifted <- (m2p >> position))
		     (p += shifted)))))
	  (if (m1 < 0) then (- p))
	  (if (m2 < 0) then (- p))
	  (prod += p)))))

