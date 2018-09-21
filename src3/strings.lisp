;;; -*- Package: user -*-
(in-package "USER")

;;::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
;; Construct for declaring a static string. The string is stored as an array of
;; ASCII characters with a null terminator. length is the number of characters that
;; in the string, but the array size is length+1 due to the trailing null.
;;::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
;(defun defstring (name value &optional (length (length value)))
(defconstruct defstring (name value &optional (length (length value)))
  (let (str)
    (setq str (pack-bytes (toascii value length)))
    
    (values `(defarray ,name ,.str) (add-string name (1+ length) env))
  )
)

;;::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
;; Generates a list of ASCII codes corresponding to the chars from the input string
;;::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
(defun toascii (string length)
  (let (str)
    (dotimes (i (1+ length))
      (cond
           ;always set last character to null and stop
           ((= i length)
            (setq str (cons 0 str))
            (return) )
       
           ;add the ASCII code of the current char to the list
           ((< i (length string))
            (setq str (cons (char-code (char string i)) str)) )
       
           ;if the input string is shorter than length, pad with zeros
           (t
            (setq str (cons 0 str)) )
      )
    )
    (values (reverse str))
  )
)
  

;;:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
;; Packs 4-byte sequences into 32-bits word using little-endian notation
;; Input: list of N integers containing values in the range 0-255
;; Output: list of CEILING(N/4) integers
;;:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
(defun pack-bytes (bytes)
  
  (let (
        (num-words (ceiling (length bytes) 4))
        (byte 0)
        (pos 0)
        (words ())
       )
    
    (dotimes (w num-words)
      (setq byte 0)
      
      (dotimes (amt 4)
        (cond
             ((< pos (length bytes))
              (setq byte (+ byte (ash (nth pos bytes) (* amt 8)))) )
         
             (t
              (return) )
        )
        
        (setq pos (1+ pos))
      )
      
      (setq words (cons byte words))
    )
    
    (reverse words)
  )
  
)
