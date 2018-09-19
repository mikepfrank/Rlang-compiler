;;; -*- Package: user -*-
(in-package "USER")

;;;----------------------------------------------------------------------
;;; This is the loader for the reversible compiler system.  Currently all
;;; files are just in the USER package.
;;; This loads a compiled version of the system compiled with CLISP on
;;; an x86 system (12/3/2001).


;; Load up the system.
(load "util.fas")             ; General-purpose utilities.
(load "infrastructure.fas")   ; Mechanism for defining and compiling constructs.
(load "location.fas")	      ; Describing locations where variables are stored.
(load "environment.fas")      ; Mapping variables to their locations.
(load "regstack.fas")         ; Direct manipulation of registers and the stack.
(load "variables.fas")        ; Creating, destroying, moving variables.
(load "branches.fas")	      ; Branches, branch pairs, labels.
(load "expression.fas")       ; Binding variables to multiply-nested expressions.
(load "clike.fas")	      ; C-like assignment-operator statements.
(load "print.fas")	      ; Data output.
(load "controlflow.fas")      ; High-level conditionals & loops.
(load "subroutines.fas")      ; Support for subroutine calls.
(load "staticdata.fas")       ; Static data definitions.
(load "program.fas")	      ; Highest-level constructs.  Standard library.
(load "library.fas")          ; Standard library of R routines.
(load "files.fas")	      ; File compiler.

(load "strings.fas")          ; String functions
(load "optimize.fas")         ; Compiler optimization functions

(load "test.fas")             ; Example programs.

;; Command to reload the system.
(defun lc () (load "loader-compiled.lisp"))			
