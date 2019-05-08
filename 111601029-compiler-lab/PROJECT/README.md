# README #

Compiler Project

### What is this repository for? ###

* This repository contains all the files involved in making of a compiler for subset of C language
* This is a part of our compiler design lab

### What does this repository contain? ###
* c.lex : this is the lexer file which converts C code into tokens
* c.grm : this is the parser file which converts tokens into abstract syntax tree
* ast.sml : contains the data structures of the abstract syntax tree
* parser.sml : this file combines the parser and lexer and takes the input c file and gives the ast data structure
* translate.sml : converts ast data structure into python code	
* driver.sml : this file takes c file as input and gives the python code
* sources.cm : make file for SML interactive mode
* ctopy.mlb : list of all files to be loaded during runtime
* Makefile : makefile for the compiler
* test_cases : 3 test cases have been added 

### Final Output ###
* The final output gets created in the file PROJECT  with the name out.py

### Project Containing/Not-Containing Features ###
* Containing Features     : IF, IF-ELSE, WHILE , and all the basic features in C including functions.
* Not Containing features : Factorial

### How to Run ###
*  ./ctopy < file name>

### By ###
* Akash Cheerla
