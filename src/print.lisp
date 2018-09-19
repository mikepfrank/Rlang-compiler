;;; -*- Package: user -*-
(in-package "USER")

(defconstruct printword (val)
  `((rawprint 0)
    (rawprint ,val)))

(defconstruct println ()
  `((rawprint 1)))

(defconstruct rawprint (val)
  (cond
   ((register? val)
    `(output ,val))
   (t
    (extract form env))))

