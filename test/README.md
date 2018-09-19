# Test files for the R compiler (`Rlang-compiler/test/`).

This subdirectory of the `Rlang-compiler` repository contains the following
source code files intended for purposes of testing the R compiler (contained
in the parallel [`src/`](../src "Source directory") directory).

* [`test.lisp`](test.lisp "Misc. test functions") - Miscellaneous functions
	and R code fragments for exercising the compiler.  Some of these may be
	obsolete.
	
* [`sch/`](sch "Schroedinger simulator") - Source code in R and sample 
	compiled PAL (Pendulum Assembly Language) code for the Schroedinger 
	wave equation simulator example.
	
  - [`Cversion-schii`](sch/Cversion-schii "C version of simulator") - Original
		implementation of the Schroedinger wave equation simulator in C.
		
  - [`sch.r`](sch/sch.r "R version of simulator") - Source code for the reversible
		implementation of the Schroedinger wave equation simulator in R.
		
  - [`sch.pal`](sch/sch.pal "Compiled PAL code") - Example output from the 
		compiler in Pendulum Assembly Language (PAL).  This code should be 
		executable in PendVM.
		
  - [`output.txt`](sch/output.txt "Example input/output") - Another example
		of input and output when running the compiler on a (slightly different)
		version of the reversible Schroedinger simulator.
