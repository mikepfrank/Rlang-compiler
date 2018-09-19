;;; -*- Package: user -*-
(in-package "USER")
;;; ---------------------------------------------------------------------
;;; A LOCATION object indicates where a variable is stored.
;;;
;;; The current implementation uses list structures.  If a <LOCATION> is
;;; NIL, then the variable exists in the environment but has no storage
;;; location (and is therefore also unbound).  If a <LOCATION> is (REG
;;; <regno>) then the variable is located in register number <REGNO>.  If
;;; <LOCATION> is (STACK <offset>) then the variable is located on the
;;; stack at the address SP+<OFFSET>, where SP is the current value of the
;;; stack pointer register.
;;; ----------------------------------------------------------------------

;; Return non-nil iff the object OBJ is a register location.
(defun register? (obj)
  (and (listp obj) (cdr obj) (null (cddr obj))
       (eq (car obj) 'reg)))

(defun stackloc? (obj)
  (and (listp obj) (cdr obj) (null (cddr obj))
       (eq (car obj) 'stack)))

;; Return non-nil iff the object OBJ is a null location (meaning the
;; location of a variable that is not located anywhere).
(defun null-loc? (obj)
  (null obj))

;; Return the given stack location's offset from the current stack
;; pointer.
(defun offset (stackloc)
  (cadr stackloc))

(defun location? (obj)
  (or (register? obj) (stackloc? obj)))
