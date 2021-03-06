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
		
  - [`branches.lisp`](branches.lisp "Support for branches") - Constructs
		providing intermediate- and low-level support for various kinds of
		branch structures for control flow.
		
  - [`expression.lisp`](expression.lisp "Expression expansion") - Constructs
		and low-level functions for expanding nested expressions.
		
  - [`clike.lisp`](clike.lisp "C-like constructs") - Defines constructs for
		various user-level C-like operators.
		
  - [`print.lisp`](print.lisp "Printing output") - Defines a few very simple
		constructs for producing output.

  - [`controlflow.lisp`](controlflow.lisp "Control flow") - Defines user-level 
		to intermediate-level control flow constructs such as conditionals
		and looping.
		
  - [`subroutines.lisp`](subroutines.lisp "Subroutine support") - Provides
		high- and low-level support for subroutines.
		
  - [`staticdata.lisp`](staticdata.lisp "Static data objects") - Defines 
		constructs for defining static data objects.  Currently this is the
		only way to provide input to a program.
		
  - [`program.lisp`](program.lisp "Whole-program constructs") - Defines very
		high-level constructs for wrapping around the entire program.
		
  - [`library.lisp`](library.lisp "Standard library") - Defines constructs that
		expand into code for standard subroutine libraries.  Currently the 
		library is very minimal.
		
* [`files.lisp`](files.lisp "File compilation") - Provides support for reading
		the source code to compile from a file.
		
Additional source code for testing purposes can be found in the [`test/`](../test "Test directory")
directory (parallel to this directory).