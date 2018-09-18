# Rlang-compiler
Compiler in Common Lisp for the "R" reversible programming language.  Copyright (C)1997 by Michael P. Frank.

## 1. Introduction

R is a reversible programming language; that is, it is a language for writing programs that can be executed in 
either the forward or the reverse time direction.  Such languages have a number of applications, such as for 
rolling back processes to synchronize concurrent systems, or for facilitating more energy-efficient hardware
implementations of algorithms.  R was historically one of the *earliest* reversible languages, predated only
by Henry Baker's Psi-Lisp, and Lutz and Derby's Janus (although R was invented independently of Janus).  The
syntax and semantics of R is vaguely C-like, but with a Lisp-like, parenthesis-heavy style of punctuation.
For more information, see the various documents in the ([`docs/` subdirectory](docs "docs/ subdirectory"))

## 2. Structure of this repository

Contents of the '`Rlang-compiler/`' repository are as follows:

### 2.1. Documentation subdirectory ([`docs/`](docs "docs/ subdirectory")).

Contains various documents describing the R language compiler, *Rcomp*, in its various iterations.

### 2.2. README file ([`README.md`](README.md "README.md file")).

This file, written in GitHub-flavored Markdown syntax.