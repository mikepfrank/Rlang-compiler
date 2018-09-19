;;; -*- Package: user -*-
(in-package "USER")

;;;----------------------------------------------------------------------
;;; File compilation code.

(defun rcompile-file (filename &key debug)
  (let (source)
    (with-open-file (stream filename)
       (loop
	(let ((next-form
	       (read stream nil :eof)))
	  (cond
	   ((eq next-form :eof)
	    (setf source (reverse source))
	    (return))
	   (t
	    (push next-form source))))))
    ;(format t "~&Source:~%")
    ;(myprint source)
    (if debug
	(rc source :debug t)
      (rc source))))
