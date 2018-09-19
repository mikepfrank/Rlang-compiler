;;; -*- Package: user -*-
(in-package "USER")

;;;----------------------------------------------------------------------
;;; Constructs for declaring static data.

;; Define NAME to refer to a static word of data in memory
;; of value VALUE.
(defconstruct defword (name value)
  `(skip
    (staticval ,name)
    (label ,name)
    (dataword ,value)))

(defconstruct defarray (name &rest elements)
  `(skip
    (staticarray ,name)
    (label ,name)
    . ,(mapcar #'(lambda (elem) `(dataword ,elem)) elements)))

;;;----------------------------------------------------------------------

;; VALUE is a word of data that should be included at
;; this point in the program in literal form.
(defconstruct dataword (value)
  `(data ,value))

(defconstruct staticval (name)
  (values nil (add-staticval name env)))

(defconstruct staticarray (name)
  (values nil (add-staticarray name env)))

;; If the flow of control gets to code surrounded by SKIP it will skip over
;; the contents without executing them.
(defconstruct skip (&body body)
  ;; Implemented by an unconditional branch pair around the body.
  `(sbra-pair ,(gentemp "_PRESKIP") ,(gentemp "_POSTSKIP")
	     . ,body))
