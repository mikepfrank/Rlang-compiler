;;; -*- Package: user -*-
(in-package "USER")

;;:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
;; Read R code from input file and writes PISA code to output file
;;:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
(defun rcompile-file (filename &key debug)
  (let (source ctime upal pal env ofile)
    
    ;read R source code and print to screen
    (with-open-file (istream filename)
      (loop
        (let ( (next-form (read istream nil :eof)) )
          (cond
            ((eq next-form :eof)
             (setf source (reverse source))
             (return))
           
            (t
             (push next-form source)) ))))
    
    (format t "~%R Source Code:")
    (myprint source t)
    

    ;compile R code and print output to screen
    (setq ctime (get-internal-run-time))
    
    (setq upal (if debug
                   (rc source :debug t)
                   (rc source)) )
    
    (setq ctime (- (get-internal-run-time) ctime))
    (setq ctime (/ ctime internal-time-units-per-second))
    (format t "~%Compile Time: ~s seconds~%" ctime)       
    
    (setq env (last upal))
    (setq upal (cdr (nbutlast upal 1)))

    (format t "~%PAL Code:~%")
    (myprint upal t)
    (format t "~%Final environment:~%")
    (print env)
    
    ;write unoptimized PAL code to .upal file in same directory as R input file
    (setq ofile (make-pathname :host (pathname-host filename)
                               :device (pathname-device filename)
                               :directory (pathname-directory filename)
                               :name (pathname-name filename)
                               :type "upal") )
      
    (with-open-file (ostream ofile :direction :output)
      (myprint upal ostream) )
      
      
    ;optimize PAL code and print output to screen
    (setq pal (peephole upal))
    
    (format t "~%Optimized PAL Code:~%")
    (myprint pal t)
    
    ;write optimized PAL code to .pal file in same directory as R input file
    (setq ofile (make-pathname :host (pathname-host filename)
                               :device (pathname-device filename)
                               :directory (pathname-directory filename)
                               :name (pathname-name filename)
                               :type "pal") )
      
    (with-open-file (ostream ofile :direction :output)
      (myprint pal ostream) )
    
  )
)
