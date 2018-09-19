;;; -*- Package: user -*-
(in-package "USER")

;;::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
;; I/O type specifiers
;; The high-order byte specifies the data type and
;; the low order byte specifies options.
;; Options are chosen by adding their values to
;; the value of the base type.
;; For example, to output and int as an unsigned hex number:
;;   (rawprint num (+ ptype-int int-unsigned int-base-16))
;;::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

;;---------------------------------
;; Integer type
;; Default: signed base 10
;;---------------------------------
(defconstant ptype-int    #x0000)

;sign representation
(defconstant int-signed     #x00)
(defconstant int-unsigned   #x01)

;base representation
(defconstant int-base-10    #x00)
(defconstant int-base-16    #x10)


;;-----------------------------------------
;; String type
;; Default: little endian
;;-----------------------------------------
(defconstant ptype-string         #x0100)

;byte-order representation
(defconstant string-little-endian   #x00)
(defconstant string-big-endian      #x01)


;;-----------------------------------------
;; Floating-point type
;; Default: fixed point notation
;;-----------------------------------------
(defconstant ptype-float   #x0200)

;notation representation
(defconstant float-fixed     #x00)
(defconstant float-exp       #x01)


;;----------------------------------
;; New line indicator
;;----------------------------------
(defconstant ptype-newline #xFF00)



(defconstruct print (val &key sign base)
  (cond
   ((or (static-val? val env)
        (numberp val) )
    (let ((type ptype-int))
      (if (eq sign 'unsigned)
        (setq type (+ type int-unsigned)) )
      (if (eq base 16)
        (setq type (+ type int-base-16)) )
    
    `((rawprint ,type)
      (rawprint ,val)) ))
   
   
   ((string? val env)
    (let (ptr)
      (dotimes (i (ceiling (get-string-length val env) 4))
        (setq ptr (append ptr (list i))) )
      
      `((rawprint ,ptype-string)
        ,(mapcar #'(lambda (p) `(rawprint (,val _ ,p))) ptr)) ))
   
   
   ((stringp val)
    `((rawprint ,ptype-string)
      ,(mapcar #'(lambda (word) `(rawprint ,word)) (pack-bytes (toascii val (length val))))) ) )
)


(defconstruct println (&optional (val nil))
  (if (null val)
    `((rawprint ,ptype-newline))
    `((print ,val) (rawprint ,ptype-newline)) )
)


(defconstruct printword (val)
  `((rawprint ,ptype-int)
    (rawprint ,val)))


(defconstruct rawprint (val)
  (cond
   ((register? val)
    `(output ,val))
   (t
    (extract form env))))


;(defconstruct println ()
;  `((rawprint 1)) )


