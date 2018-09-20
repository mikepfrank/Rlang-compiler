# The R language compiler (`Rlang-compiler`).

This repository provides Common Lisp source code for the compiler (*Rcomp*) for the *R* reversible 
programming language.  Copyright (C)1997-2018 by Michael P. Frank.

## 1. Introduction.

R is a *reversible* programming language; that is, it is a high-level language for writing programs that can 
be executed in either the forward or the reverse time direction.  Such languages have a number of applications, 
such as for rolling back processes to synchronize concurrent systems, or for facilitating more energy-efficient 
hardware implementations of algorithms.  

Created in 1997 [1], R was historically one of the *earliest* reversible high-level languages, 
predated only by Lutz and Derby's Janus [2] in 1982, and Henry Baker's Psi-Lisp [3] in 1992.
Although R was actually developed independently of Janus, we ended up reinventing some of the 
same concepts.  

The syntax and semantics of R is vaguely C-like, but with a Lisp-like, parenthesis-heavy 
style of punctuation.  For more information, see the various documents contained in 
the [`docs/`](docs "docs/ subdirectory") subdirectory.

Please note that *R* is unrelated to the statistics language of the same name.  When our R was named, in 1997, 
the other one was not yet widely known.  Since then, the R statistics language has become very popular, so it
might be a good idea to rename the R reversible language at some point to avoid confusion.  One alternate name 
that I have used for it occasionally is "ЯR" (pronounced "yar"; the backwards R is Cyrillic "ya").  In ASCII 
this can be rendered "YaR" and taken to mean "Yet another R" or "Yet another Reversible (language)."

## 2. Requirements.

*Rcomp* is written in the [Common Lisp](https://en.wikipedia.org/wiki/Common_Lisp "CL wiki page") programming 
language, so running it requires a Common Lisp development environment (interpreter and/or compiler).  An example
environment of this sort that works well is [CLISP](https://clisp.sourceforge.io/ "CLISP SourceForge"), which is 
available on SourceForge, and also is an option in package distribution systems for common Unix-like environments, 
such as [CygWin](https://www.cygwin.com/ "CygWin home page").

## 3.  A quick tutorial.

With CLISP installed in your shell environment, `cd` to the `src2/` subdirectory of this repository's file hierarchy, and 
type `clisp`; you will see a welcome screen something like this:

      i i i i i i i       ooooo    o        ooooooo   ooooo   ooooo
      I I I I I I I      8     8   8           8     8     o  8    8
      I  \ `+' /  I      8         8           8     8        8    8
       \  `-+-'  /       8         8           8      ooooo   8oooo
        `-__|__-'        8         8           8           8  8
            |            8     o   8           8     o     8  8
      ------+------       ooooo    8oooooo  ooo8ooo   ooooo   8

    Welcome to GNU CLISP 2.49+ (2010-07-17) <http://clisp.org/>

    Copyright (c) Bruno Haible, Michael Stoll 1992, 1993
    Copyright (c) Bruno Haible, Marcus Daniels 1994-1997
    Copyright (c) Bruno Haible, Pierpaolo Bernardi, Sam Steingold 1998
    Copyright (c) Bruno Haible, Sam Steingold 1999-2000
    Copyright (c) Sam Steingold, Bruno Haible 2001-2010

    Type :h and hit Enter for context help.

    [1]> 

Then load the *Rcomp* compiler by entering `(load "loader.lisp")`.  

    ;; Loading file loader.lisp ...
    ;;  Loading file util.lisp ...
    ;;  Loaded file util.lisp
    ;;  Loading file infrastructure.lisp ...
    ;;  Loaded file infrastructure.lisp
    ;;  Loading file location.lisp ...
    ;;  Loaded file location.lisp
    ;;  Loading file environment.lisp ...
    ;;  Loaded file environment.lisp
    ;;  Loading file regstack.lisp ...
    ;;  Loaded file regstack.lisp
    ;;  Loading file variables.lisp ...
    ;;  Loaded file variables.lisp
    ;;  Loading file branches.lisp ...
    ;;  Loaded file branches.lisp
    ;;  Loading file expression.lisp ...
    ;;  Loaded file expression.lisp
    ;;  Loading file clike.lisp ...
    ;;  Loaded file clike.lisp
    ;;  Loading file print.lisp ...
    ;;  Loaded file print.lisp
    ;;  Loading file controlflow.lisp ...
    ;;  Loaded file controlflow.lisp
    ;;  Loading file subroutines.lisp ...
    ;;  Loaded file subroutines.lisp
    ;;  Loading file staticdata.lisp ...
    ;;  Loaded file staticdata.lisp
    ;;  Loading file program.lisp ...
    ;;  Loaded file program.lisp
    ;;  Loading file library.lisp ...
    ;;  Loaded file library.lisp
    ;;  Loading file files.lisp ...
    ;;  Loaded file files.lisp
    ;;  Loading file strings.lisp ...
    ;;  Loaded file strings.lisp
    ;;  Loading file optimize.lisp ...
    ;;  Loaded file optimize.lisp
    ;;  Loading file test.lisp ...
    ;;  Loaded file test.lisp
    ;; Loaded file loader.lisp
    #P"/cygdrive/c/Users/mikep/Documents/GitHub/Rlang-compiler/src2/loader.lisp"
    [2]> 

At this point, you can interactively enter a short program to compile:

    (myprint
        (rcomp
            '(  (defstring hi "Hello, world!")
                (defmain hello
                    (println hi))   )))
                    
This produces the following output in PAL (Pendulum Assembly Language).  (Which clearly needs to be more optimized.)

    _PRESKIP97:     BRA _POSTSKIP98
    HI:             DATA 1819043144
                    DATA 1998597231
                    DATA 1684828783
                    DATA 33
    _POSTSKIP98:    BRA _PRESKIP97
    _MAINTOP:       BRA _MAINBOT
                    .START HELLO
    HELLO:          START
                    ADDI $2 256
                    OUTPUT $2
                    ADDI $2 -256
                    ADDI $3 HI
                    EXCH $4 $3
                    ADD $2 $4
                    EXCH $4 $3
                    ADDI $3 -HI
                    OUTPUT $2
                    ADDI $3 HI
                    EXCH $4 $3
                    SUB $2 $4
                    EXCH $4 $3
                    ADDI $3 -HI
                    ADDI $3 HI
                    ADDI $3 1
                    EXCH $4 $3
                    ADD $2 $4
                    EXCH $4 $3
                    ADDI $3 -1
                    ADDI $3 -HI
                    OUTPUT $2
                    ADDI $3 HI
                    ADDI $3 1
                    EXCH $4 $3
                    SUB $2 $4
                    EXCH $4 $3
                    ADDI $3 -1
                    ADDI $3 -HI
                    ADDI $3 HI
                    ADDI $3 2
                    EXCH $4 $3
                    ADD $2 $4
                    EXCH $4 $3
                    ADDI $3 -2
                    ADDI $3 -HI
                    OUTPUT $2
                    ADDI $3 HI
                    ADDI $3 2
                    EXCH $4 $3
                    SUB $2 $4
                    EXCH $4 $3
                    ADDI $3 -2
                    ADDI $3 -HI
                    ADDI $3 HI
                    ADDI $3 3
                    EXCH $4 $3
                    ADD $2 $4
                    EXCH $4 $3
                    ADDI $3 -3
                    ADDI $3 -HI
                    OUTPUT $2
                    ADDI $3 HI
                    ADDI $3 3
                    EXCH $4 $3
                    SUB $2 $4
                    EXCH $4 $3
                    ADDI $3 -3
                    ADDI $3 -HI
                    ADDI $2 65280
                    OUTPUT $2
                    ADDI $2 -65280
                    FINISH
    _MAINBOT:       BRA _MAINTOP

For additional examples, see the various documentation files and test files.
    
## 3. Structure of this repository.

Contents of the '`Rlang-compiler/`' repository are as follows:

### 3.1.  Documentation subdirectory ([`docs/`](docs "docs/ subdirectory")).

Contains various documents describing the R language compiler, *Rcomp*, in its various iterations.

### 3.2.  Original source code subdirectory ([`src/`](src "src/ subdirectory")).

This directory contains the complete source code for the original version of the R compiler,
which was developed in 1997 (with minor revisions in 2001) by M. Frank.

### 3.3.  Improved source code subdirectory ([`src2/`](src2 "src2/ subdirectory")).

This directory contains a version of the R language compiler modified by University of Florida student 
Chris Clark for his senior project during the Fall semester of 2001.  The ([`docs/`](docs "docs/ subdirectory"))
subdirectory contains Chris' final report which describes the details of the changes made to the compiler.

### 3.4.  Test code subdirectory ([`test/`](test "test/ subdirectory")).

This directory contains various files intended for purposes of testing/exercising the compiler.

### 3.5. README file ([`README.md`](README.md "README.md file")).

This file, written in GitHub-flavored Markdown syntax.

### 3.6.  To-Do file ([`TO-DO.txt`](TO-DO.txt "To-Do text file")).

Notes on things to do with this repository.

## References:

[1] Michael P. Frank, "[The R Programming Language and Compiler](docs/MIT-RCP-MemoM8-RProgLang.pdf "Memo #M8")," 
MIT Reversible Computing Project Memo #M8, MIT AI Lab, July 1997.

[2] Chris Lutz, [*letter to Rolf Landauer*](http://revcomp.info/legacy/mpf/rc/janus.html "Letter describing Janus"), Apr. 1986.
[`http://revcomp.info/legacy/mpf/rc/janus.html`](http://revcomp.info/legacy/mpf/rc/janus.html "Letter describing Janus")

[3] Henry G. Baker, "NREVERSAL of fortune---the thermodynamics of garbage collection," in Y. Bekkers, ed., 
International Workshop on Memory Management, pp. 507-524. Springer-Verlag, 1992.

