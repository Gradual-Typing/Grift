#lang scribble/manual

@(require setup/getinfo)
@(define nfo (get-info '("Schml")))
@(define vers (format "~a" (nfo 'version)))

@title[#:version @vers]{The Schml Programming Language & Compiler}

@section{The Schml Programming Language}
This section lays out a description of the language
as a whole. Introducing core forms, macros, semantics, etc.


@section{The Schml Compiler}
This section documents how the compiler is set up and every
pass.


