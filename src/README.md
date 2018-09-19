# Original source code for the R language compiler (`Rlang-compiler/src/`).

This subdirectory of the `Rlang-compiler` repository contains the following
source code files for the original version of R compiler *Rcomp*, written in 
Common Lisp by M. Frank in 1997 (with minor modifications through 2001).

* [`loader.lisp`](loader.lisp "System loader") - This loads up all parts of 
	the *Rcomp* program in an appropriate order.

* [`util.lisp`](util.lisp "Utilities") - Defines general-purpose utility
	functions and macros that we use.

* [`infrastructure.lisp`](infrastructure.lisp "Compilation infrastructure") -
	Defines our macro-expansion-like facility for defining how to compile
	language constructs.

* [`location.lisp`](location.lisp "Location objects") - Defines some functions
	for working with objects that describe a variable's location in the 
	register file or the stack.
	
