;;; -*- Package: user -*-
(in-package "USER")

;;;----------------------------------------------------------------------
;;; This is the loader for the reversible compiler system.  Currently all
;;; files are just in the USER package.

;; Load up the system.
(load "util.lisp")            ; General-purpose utilities.
(load "infrastructure.lisp")  ; Mechanism for defining and compiling constructs.
(load "location.lisp")	      ; Describing locations where variables are stored.
(load "environment.lisp")     ; Mapping variables to their locations.
(load "regstack.lisp")        ; Direct manipulation of registers and the stack.
(load "variables.lisp")       ; Creating, destroying, moving variables.
(load "branches.lisp")	      ; Branches, branch pairs, labels.
(load "expression.lisp")      ; Binding variables to multiply-nested expressions.
(load "clike.lisp")	      ; C-like assignment-operator statements.
(load "print.lisp")	      ; Data output.
(load "controlflow.lisp")     ; High-level conditionals & loops.
(load "subroutines.lisp")     ; Support for subroutine calls.
(load "staticdata.lisp")      ; Static data definitions.
(load "program.lisp")	      ; Highest-level constructs.  Standard library.
(load "library.lisp")         ; Standard library of R routines.
(load "files.lisp")	      ; File compiler.

(load "test.lisp")            ; Example programs.

;; Command to reload the system.
(defun l () (load "loader.lisp"))			
