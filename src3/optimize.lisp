;;; -*- Package: user -*-
(in-package "USER")

;;:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
;; Scans the PAL output of the compiler looking for inefficient code sequnces
;; and removes or replaces them with more efficient equivalents.
;;:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
(defun peephole (pal)
  
  (let ((pal-above nil)
        (pal-below pal)
        (inst nil)
        (next-inst nil) )
  
    ;(do ((pal-below (cdr pal) (cdr pal-below))) (null pal-below)
    ;iterate through each line of the pal code looking for optimizations
    (loop
      (setq inst (first pal-below))
      (setq next-inst (second pal-below))
      (format t "~%Reading: ~20T~A~%~20T~A~%" inst next-inst)
      
      (cond
           ; Stop when the end of the code is reached
           ((null inst)
            (return) )

           ;:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
           ; Remove sequential inverse ADDI operations of literals
           ; Ex: ADDI $2 1
           ;     ADDI $2 -1
           ;:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
           ((and (listp inst)
                 (listp next-inst)
                 (equal (first inst) 'ADDI)
                 (equal (first next-inst) 'ADDI)
                 (equal (second inst) (second next-inst))
                 (atom (third inst))
                 (atom (third next-inst))
                 (numberp (third inst))
                 (numberp (third next-inst))
                 (equal (third inst) (- (third next-inst))) )
            
            ;remove sequence and go back to previous inst on next iteration
            (format t "Removing sequence: ~20T~A~%~20T~A~%" inst next-inst)
            (setq pal-below (cons (first pal-above) (rest (rest pal-below))))
            (setq pal-above (rest pal-above)) )
      
      
           ;:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
           ; Remove sequential inverse ADDI operations of data items
           ; Ex: ADDI $2 X
           ;     ADDI $2 -X
           ;:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
           ((and (listp inst)
                 (listp next-inst)
                 (equal (first inst) 'ADDI)
                 (equal (first next-inst) 'ADDI)
                 (equal (second inst) (second next-inst))
                 (atom (third inst))
                 (atom (third next-inst))
                 (not (numberp (third inst)))
                 (not (numberp (third next-inst)))
                 (or (string= (third inst) (third next-inst) :start1 1)
                     (string= (third inst) (third next-inst) :start2 1) ))
            
            ;remove sequence and go back to previous inst on next iteration
            (format t "Removing sequence: ~20T~A~%~20T~A~%" inst next-inst)
            (setq pal-below (cons (first pal-above) (rest (rest pal-below))))
            (setq pal-above (rest pal-above)) )
      
       
           ;:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
           ; Remove sequential inverse ADD/SUB operations
           ; Ex: ADD $2 $3
           ;     SUB $2 $3
           ;:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
           ((and (listp inst)
                 (listp next-inst)
                 (or (and (equal (first inst) 'ADD) (equal (first next-inst) 'SUB))
                     (and (equal (first inst) 'SUB) (equal (first next-inst) 'ADD)) )
                 (equal (second inst) (second next-inst))
                 (equal (third inst) (third next-inst)) )
            
            ;remove sequence and go back to previous inst on next iteration
            (format t "Removing sequence: ~20T~A~%~20T~A~%" inst next-inst)
            (setq pal-below (cons (first pal-above) (rest (rest pal-below))))
            (setq pal-above (rest pal-above)) )


           ;:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
           ; Remove sequential duplicate EXCH operations
           ; Ex: EXCH $2 $3
           ;     EXCH $2 $3
           ;:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
           ((and (listp inst)
                 (listp next-inst)
                 (equal (first inst) 'EXCH)
                 (equal (first next-inst) 'EXCH)
                 (equal (second inst) (second next-inst))
                 (equal (third inst) (third next-inst)) )
            
            ;remove sequence and go back to previous inst on next iteration
            (format t "Removing sequence: ~20T~A~%~20T~A~%" inst next-inst)
            (setq pal-below (cons (first pal-above) (rest (rest pal-below))))
            (setq pal-above (rest pal-above)) )
       
       
           ;:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
           ; Remove unnecessary branch instructions around static data
           ; caused by sequencial declarations
           ; Ex: _PRESKIP1:   BRA _POSTSKIP2  ;skip X and Y - change to BRA _POSTSKIP4
           ;     X:           DATA 0
           ;     _POSTSKIP2:  BRA _PRESKIP1   ;unnecessary - eliminate
           ;     _PRESKIP3:   BRA _POSTSKIP4  ;unnecessary - eliminate
           ;     Y:           DATA 1
           ;     _POSTSKIP4:  BRA _PRESKIP3   ;change to BRA _PRESKIP1
           ;:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
           ((and (atom inst)
                 (> (length (string inst)) 8)
                 (string= inst "_PRESKIP" :end1 8) )
            
            ;found a static data item, so extract all consecutive static data declarations
            (let ((sdata ()) (top-target nil) (bot-target nil))
              (do () 
                  ;until
                  ((not (and (atom (first pal-below))
                             (> (length (string (first pal-below))) 8)
                             (string= (first pal-below) "_PRESKIP" :end1 8) )))
                
                (setq sdata (cons (remove-inst pal-below) sdata))  ;get top branch label
                (setq sdata (cons (remove-inst pal-below) sdata))  ;get top BRA inst
                (setq sdata (cons (remove-inst pal-below) sdata))  ;get data label
                (dolist (i pal-below)                              ;get all data statements
                  (cond ((and (listp i)
                              (equal (car i) 'DATA) )
                         (setq sdata (cons (remove-inst pal-below) sdata)) )
                      
                        (t
                         (return) )
                  )
                )
                (setq sdata (cons (remove-inst pal-below) sdata))  ;get bottom branch label
                (setq sdata (cons (remove-inst pal-below) sdata))  ;get bottom BRA inst
                
              )
              (format t "~%Optimizing static data sequence:~%")
              (format t "~A" (myprint (reverse sdata)))
              
              ;label at the bottom of the data section is the next-to-last item in the sequence
              (setq bot-target (second sdata))
              
              ;make the first item in the sequence be the first item of the list
              (setq sdata (reverse sdata))
              
              ;label at the top of the data section is now the first item in the list
              (setq top-target (first sdata))
              
              (format t "~%Top: ~A~%Bottom: ~A~%" top-target bot-target)
               
              ; modify and copy static data sequence to processed list
              
              (setq pal-above (cons (remove-inst sdata) pal-above))  ;top label is the same
              (setq pal-above (cons `(BRA ,bot-target) pal-above))   ;change BRA inst to point to end of data section
              (remove-inst sdata)                                    ;ignore old BRA inst
              
              (loop
                (cond
                     ;Stop when the end of the sequence is reached
                     ((null sdata)
                       (return) )

                     ;Deal with branch labels
                     ((and (atom (first sdata))
                           (> (length (string (first sdata))) 8)
                           (string= (first sdata) "_POSTSKIP" :end1 9) )
                      
                      (cond
                           ;Remove intermediate branches
                           ((string/= (first sdata) bot-target)
                            (format t "Found intermediate branch: ~A~%" (first sdata))
                            (remove-inst sdata)   ;remove POST label
                            (remove-inst sdata)   ;remove POST BRA inst
                            (remove-inst sdata)   ;remove PRE label
                            (remove-inst sdata) ) ;remove PRE BRA inst
                       
                           ;Modify bottom branch
                           ((string= (first sdata) bot-target)
                            (format t "Found bottom branch: ~A~%" (first sdata))
                            (setq pal-above (cons (remove-inst sdata) pal-above))  ;bottom label is the same
                            (setq pal-above (cons `(BRA ,top-target) pal-above))   ;change BRA inst to point to top of data section
                            (remove-inst sdata) )                                  ;ignore old BRA inst
                      )
                    )

                    ;Move all other items without modification
                    (t
                     (format t "Moving item: ~A~%" (first sdata))
                     (setq pal-above (cons (remove-inst sdata) pal-above)) )
                )
              )

            )
           )
       
       
       
          ;No optimization found for this instruction, so move it to processed list
          (t
           (setq pal-above (cons inst pal-above))
           (setq pal-below (rest pal-below)) )
      )
      
    )
    
    ;(format t "~A~%~A~%" (myprint pal) (myprint (reverse pal-above)))
    
    (values (reverse pal-above))
  )
  
)


;;:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
;; Removes the first item from the list and returns it 
;;:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
(defmacro remove-inst (pal)
  `(prog1 (first ,pal)
          (setq ,pal (rest ,pal)) )
)
