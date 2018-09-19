# Documentation for the R language compiler (`Rlang-compiler/docs/`).

This subdirectory of the `Rlang-compiler` repository contains the following
documents pertaining to the R language compiler in its various iterations.

## "The R Programming Language and Compiler" (M. Frank, July 1997) ([`MIT-RCP-MemoM8-RProgLang.pdf`](MIT-RCP-MemoM8-RProgLang.pdf "MIT RCP Memo #M8")).

This research memo, which was produced as part of the MIT Reversible Computing Project, 
which ran from approximately 1996-1999 in the MIT AI Lab, was the original documentation 
for the R programming language and its compiler.  This material (along with more details
about the compiler internals) was later recycled into my doctoral dissertation (below).

## _Reversibility for Efficient Computing_ (M. Frank, Dec. 1999) ([`Frank-99-PhD-bookmarked.pdf`](Frank-99-PhD-bookmarked.pdf "dissertation PDF")).

This document is the unofficial (but *unabridged*) revised manuscript of M. Frank's (*i.e.*, my) 1999 
MIT doctoral dissertation.  I published this extended version (which includes some material that was
removed from the official version that filed with the MIT Libraries) online in the Fall of 1999 at 
the University of Florida after I started my faculty position there.

The sections of this document that pertain to the R language include:

* Section 9.4, "Reversible programming languages," pp. 236-239.
	
* Section 9.5.6, "Physical simulations," pp. 241-243. (Describes an example application.)
	
* Appendix C, "The R reversible programming language," pp. 295-310.  A complete 
reference for the user-visible R language constructs.
	
* Appendix D, "The R language compiler," pp. 311-376.  A complete reference for  
the compiler's intermediate language constructs; also includes complete source 
code for the compiler.  (Also in the [`src/`](../src "/src subdirectory") subdirectory,
parallel to this directory.)
	
* Appendix E, "Reversible Schroedinger wave simulation," pp. 377-406.  Includes
source code and complete compiled output in the PISA assembly language.

## "Improving the reversible programming language R and its supporting tools" (C. Clark, Dec. 2001) ([`Clark_2001-12_SrPrj-report.pdf`](Clark_2001-12_SrPrj-report.pdf "Clark senior project report")).

This document was the final report for a Senior Design project conducted by Christopher R. Clark at the 
University of Florida CISE department in 2001.  In this project, Chris extended the R language to add
new features such as support for `else` clauses and string literals.  The modified code can be found in the 
[`src2/`](../src2 "/src2 subdirectory") subdirectory (parallel to this `docs/` directory).

