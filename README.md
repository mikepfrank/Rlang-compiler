# The R language compiler (`Rlang-compiler`).

This repository provides Common Lisp source code for the compiler for the *R* reversible programming language.  
Copyright (C)1997-2018 by Michael P. Frank.

## 1. Introduction.

R is a *reversible* programming language; that is, it is a high-level language for writing programs that can 
be executed in either the forward or the reverse time direction.  Such languages have a number of applications, 
such as for rolling back processes to synchronize concurrent systems, or for facilitating more energy-efficient 
hardware implementations of algorithms.  

Created in 1997 [1], R was historically one of the *earliest* reversible high-level languages, 
predated only by Lutz and Derby's Janus [2] in 1982, and Henry Baker's Psi-Lisp [3] in 1992 (although R was 
invented independently of Janus, we reinvented some of the same concepts).  

The syntax and semantics of R is vaguely C-like, but with a Lisp-like, parenthesis-heavy 
style of punctuation.  For more information, see the various documents contained in 
the ([`docs/` subdirectory](docs "docs/ subdirectory"))

## 2. Structure of this repository.

Contents of the '`Rlang-compiler/`' repository are as follows:

### 2.1.  Documentation subdirectory ([`docs/`](docs "docs/ subdirectory")).

Contains various documents describing the R language compiler, *Rcomp*, in its various iterations.

### 2.2.  Original source code subdirectory ([`src/`](src "src/ subdirectory")).

This directory contains the complete source code for the original version of the R compiler,
which was developed in 1997 (with minor revisions in 2001) by M. Frank.

### 2.3.  Improved source code subdirectory ([`src2/`](src2 "src2/ subdirectory")).

This directory contains a version of the R language compiler modified by University of Florida student 
Chris Clark for his senior project during the Fall semester of 2001.  The ([`docs/`](docs "docs/ subdirectory"))
subdirectory contains Chris' final report which describes the details of the changes made to the compiler.

### 2.4.  Test code subdirectory ([`test/`](test "test/ subdirectory")).

This directory contains various files intended for purposes of testing/exercising the compiler.

### 2.5. README file ([`README.md`](README.md "README.md file")).

This file, written in GitHub-flavored Markdown syntax.

## References:

[1] Michael P. Frank, "[The R Programming Language and Compiler](docs/MIT-RCP-MemoM8-RProgLang.pdf "Memo #M8")," 
MIT Reversible Computing Project Memo #M8, MIT AI Lab, July 1997.

[2] Chris Lutz, [*letter to Rolf Landauer*](http://revcomp.info/legacy/mpf/rc/janus.html "Letter describing Janus"), Apr. 1986.
[`http://revcomp.info/legacy/mpf/rc/janus.html`](http://revcomp.info/legacy/mpf/rc/janus.html "Letter describing Janus")

[3] Henry G. Baker, "NREVERSAL of fortune---the thermodynamics of garbage collection," in Y. Bekkers, ed., 
International Workshop on Memory Management, pp. 507-524. Springer-Verlag, 1992.

