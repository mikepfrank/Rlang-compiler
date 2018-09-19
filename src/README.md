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
	
* [`environment.lisp`](environment.lisp "Environment objects") - Defines the 
	environment objects which map variables to their locations.

* Files defining "macros" for expansion of language constructs:

  - [`regstack.lisp`](regstack.lisp "Register/stack manipulation") - Defines
		low-level constructs for direct manipulation of registers and the 
		stack.

  - [`variables.lisp`](variables.lisp "Manipulation of variables") - Defines
		high- to low-level constructs for manipulation of variables in
		variable assignments (environments).
		

