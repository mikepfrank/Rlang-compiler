;;; -*- Package: user -*-
(in-package "USER")

;;----------------------------------------------------------------------
;;; Highest-level constructs.

(defconstruct defmain (progname &body body)
  `(;; Always include the standard library of subroutines.
    ;(standard-library)
    ;; We surround the whole program with a branch pair because I don't
    ;; think that our current idea of START/FINISH boundary instructions
    ;; can be non-noops on the real machine without dissipation. This
    ;; also skips over main if control somehow comes down from above.
    (twin-us-branch _MAINTOP _MAINBOT
      ;; Execution starts and ends with exactly 0 dynamic variables.
      (with-location-map ,(empty-locmap)
        (declare-startpoint ,progname)
        ;; To begin execution, the PC should initally be set to this label.
        (label ,progname)
	(start)
        ,@body
	(finish)))))

;;----------------------------------------------------------------------

;; Defprog: a whole program with subroutines and a main routine.
;; Now deprecated in favor of defsub + defmain (6/26/97).
(defconstruct defprog (progname subs &body main)
  `(;; Always include the standard library of subroutines.
    (standard-library)
    ;; Include user subroutines.
    ,@subs
    ;; We surround the whole program with a branch pair because I don't
    ;; think that our current idea of START/FINISH boundary instructions
    ;; can be implemented without dissipation.
    (twin-us-branch _MAINTOP _MAINBOT
     ;; Execution starts and ends with exactly nothing in the environment.
     (with-location-map ,(empty-locmap)
       (declare-startpoint ,progname)
       ;; To begin execution, the PC should initally be set to this label.
       (label ,progname)
       (start)
       ,@main
       (finish)))))

;;----------------------------------------------------------------------

(defconstruct declare-startpoint (labname)
  `(.start ,labname))
