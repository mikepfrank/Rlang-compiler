;;; -*- Package: user -*-
(in-package "USER")
;;; ======================================================================
;;; This file defines the interface to and implementation of ENVIRONMENT
;;; objects.  An environment object determines what variables are present
;;; in the R environment at a given point in the program, and where they
;;; are stored.  The environment also maintains identifiers for static
;;; objects.  This file provides the programmer's interface to environment
;;; objects, and should be used in lieu of manipulating the underlying
;;; structures directly.  This is intended to reduce errors and allow
;;; environments to be reimplemented at a later time.
;;; ======================================================================

(defun empty-locmap () '())

(defclass environment ()
  ((variable-locations
    :type list ;More specifically an ALIST from identifiers to locations.
    :initform (empty-locmap)
    :initarg :locmap
    :accessor locmap
    :documentation "An ALIST of the form ((<var1> . <location1>) ...).
       Each VAR is a symbol, and only appears once in the alist.  The
       variables that have most recently been created or moved appear at
       the front of the alist.")
   (static-value-identifiers
    :type list
    :initform nil
    :initarg :staticvals
    :accessor staticvals
    :documentation "A list of identifier symbols that denote static
       data values permanently located in memory.")
   (static-array-identifiers
    :type list
    :initform nil
    :initarg :staticarrays
    :accessor staticarrays
    :documentation "A list of identifier symbols denoting static arrays.")
   )
  (:documentation "An environment specifies the meanings of identifiers at
     a given point during the compilation of a program."))

(defmacro make-environment (&rest args)
  `(make-instance 'environment . ,args))

(defun empty-env ()
  (make-environment))

(defun copy-environment (env)
  (make-environment :locmap (copy-alist (locmap env))
		    :staticvals (copy-list (staticvals env))
		    :staticarrays (copy-list (staticarrays env))))

(defmethod env-to-list ((env environment))
  `(:locmap ,(locmap env)
    :staticvals ,(staticvals env)
    :staticarrays ,(staticarrays env)))

(defmethod print-object ((env environment) stream)
  (write (env-to-list env) :stream stream))

;; Return an environment like ENV, but with VAR bound to location LOC
;; in the location map.
(defmethod set-loc (var loc (env environment))
  (setf env (copy-environment env)
	(locmap env) `((,var .,loc).,(remove (assoc var (locmap env))
					     (locmap env))))
  env)

;; Return an environment that is just like the given environment ENV but
;; with the variable VAR removed from the location map.
(defmethod remove-var (var (env environment))
  (setf env (copy-environment env) 
	(locmap env) (remove (assoc var (locmap env)) (locmap env)))
  env)

;; Return non-nil iff the variable VAR exists in the environment ENV.
(defmethod defined-in-env? (var (env environment))
  (assoc var (locmap env)))

;; Return the location of variable VAR in environment ENV, or nil if VAR
;; does not exist in the environment.  This is not guaranteed to be
;; distinct from the null location.  (The DEFINED-IN-ENV function can be
;; used to distinguish the two cases.)
(defmethod location (var (env environment))
  (cdr (assoc var (locmap env))))

;; Return the variable stored at the given location in the given
;; environment, or nil if none.
(defmethod var-at-loc (loc (env environment))
  (car (rassoc loc (locmap env) :test #'equal)))

;; Return non-nil iff the two environments E1 and E2 contain the exact same
;; set of variables.
(defmethod equal-vars? ((e1 environment) (e2 environment))
  (let ((answer t))
    (dolist (v (append (mapcar #'car (locmap e1)) (mapcar #'car (locmap e2))))
	(if (not (and (assoc v (locmap e1)) (assoc v (locmap e2))))
	    (setf answer nil)))
    answer))

;; Return non-nil iff the two environments E1 and E2 are equivalent, in the
;; sense that they have the same variables and assign them to the same
;; locations.
(defmethod equal-env? ((e1 environment) (e2 environment))
  (let ((answer t))
    (dolist (v (append (mapcar #'car (locmap e1)) (mapcar #'car (locmap e2))))
      (if (not (equal (assoc v (locmap e1)) (assoc v (locmap e2))))
	  (setf answer nil)))
    answer))

;; Return the first variable in environment E1 that is not located in the
;; same place in environment E2.
(defmethod first-misloc ((e1 environment) (e2 environment))
  (dolist (v (mapcar #'car (locmap e1)))
    (if (not (equal (cdr (assoc v (locmap e1))) (cdr (assoc v (locmap e2)))))
	(return v))))

;; Return the lowest-numbered available register location in the given
;; environment.  Available means not containing any variable.  Registers 0
;; and 1 are never available because 0 is reserved to contain 0 and 1 is
;; reserved to be the stack pointer.  Returns nil if no registers are
;; available.
(defmethod next-avail-reg ((env environment))
  (let ((i 2))
    (loop
     (if (not (rassoc `(reg ,i) (locmap env) :test #'equal))
	 (return `(reg ,i)))
     (incf i)
     (if (= i 32) (return)))))

;; Return the first stack location ABOVE THE CURRENT STACK
;; POINTER that is available (doesn't contain a variable) in the given
;; environment.  The stack grows down, so ABOVE means MORE NEGATIVE THAN.
(defmethod next-avail-stack ((env environment))
  (let ((i -1))
    (loop
     (if (not (rassoc `(stack ,i) (locmap env) :test #'equal))
	 (return `(stack ,i)))
     (decf i))))

;; Return a variable in the given environment that was least recently
;; created or moved.
(defmethod least-recently-moved ((env environment))
  ;; Currently this information is maintained by putting newly-created or
  ;; moved variables on the front of the alist, so we just return the last
  ;; variable on the list.
  (car (nth (1- (length (locmap env))) (locmap env))))

;; Return non-nil if VAR is located in a register in environment ENV.
(defmethod in-register? (var (env environment))
  (register? (location var env)))

;; Return the index of the topmost stack location at SP or below that
;; has a variable in it.  In other words, where is
;; the top of the stack in relation to the current stack pointer.
(defmethod top-of-stack ((env environment))
  (let ((h 0))
    (dolist (loc (locmap env))
      (if (eq (cadr loc) 'stack)
	  (if (< (caddr loc) h)
	      (setf h (caddr loc)))))
    h))

(defmethod add-staticval (name (env environment))
  (setf env (copy-environment env)
	(staticvals env) (cons name (staticvals env)))
  env)

(defmethod add-staticarray (name (env environment))
  (setf env (copy-environment env)
	(staticarrays env) (cons name (staticarrays env)))
  env)

(defmethod static-id? (obj (env environment))
  (and (symbolp obj)
       (or (member obj (staticvals env))
	   (member obj (staticarrays env)))))

(defmethod dynamic-var? (obj (env environment))
  (and (symbolp obj)
       (not (static-id? obj env))))

(defmethod static-array? (obj (env environment))
  (member obj (staticarrays env)))

(defun negated-sym? (obj)
  (and (symbolp obj)
       (eq #\- (elt (symbol-name obj) 0))))

(defun positive-of (negsym)
  (intern (subseq (symbol-name negsym) 1)))

(defmethod literal? (obj (env environment))
  (or (numberp obj)
      ;; Static array identifiers are literals because they stand for their
      ;; addresses, and are left in the form of symbols which are processed
      ;; directly by the assembler. 
      (static-array? obj env)
      ;; If FOO is a static array, -FOO is a literal also.
      (and (negated-sym? obj)
	   (static-array? (positive-of obj) env))
      ;; Expression is the address of a static-val.
      (static-val-addr? obj env)))

(defmethod static-val? (obj (env environment))
  (member obj (staticvals env)))

(defmethod static-val-addr? (obj (env environment))
  (and (listp obj)
       (eq (car obj) '&)
       (null (cddr obj))
       (static-val? (second obj) env)))