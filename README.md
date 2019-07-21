# PaComb: grammar and combinators for OCaml

## Overview

PaComb implements a representation of grammars with semantical action (i.e. a
value returned as result of parsing).  Parsing is performed by compiling the
grammar to combinators implemented in the [Combinator] module. This library
offers "scanner less" parsing, but the [Lex] module provide a notion of
terminals and blanks which allows for easy way to write grammars in two
phases as usual.

Defining languages using directly the Grammar module leads to cumbersome
code. This is why Pacomb propose a ppx extension that can be used with the
compilation flag [-ppx pacombPpx]. Here is an example:


```
    [%%parser
       type p = Atom | Prod | Sum
       let rec
         expr p = Atom < Prod < Sum
                ; (p=Atom) (x::FLOAT)                        => x
                ; (p=Atom) '(' (e::expr Sum) ')'             => e
                ; (p=Prod) (x::expr Prod) '*' (y::expr Atom) => x*.y
                ; (p=Prod) (x::expr Prod) '/' (y::expr Atom) => x/.y
                ; (p=Sum ) (x::expr Sum ) '+' (y::expr Prod) => x+.y
                ; (p=Sum ) (x::expr Sum ) '-' (y::expr Prod) => x-.y
    ]
```

The extension [[%%parser ...]] extends structure with new let bindings
defining grammars. This applies both for [let] and [let rec] the latter being
reserved to recursive grammars.  We also provide an extension [[%grammar]]
for expression that corresponds to grammars, i.e.  the right-hand side of
binding in the [[%%parser]] extension.

Here is the BNF for these right-hand-side, with its semantics

```
    grammar ::= rule                                                   itself
           | grammar ; rule                                       Grammar.alt
    rule ::= qitems => expr                            A rule with its action
           | expr < ... < expr                       priority order see below
    qitems ::= ()                                               Grammar.empty
           | non_empty_qitems                                          itself
    non_empty_qitems ::= qitem
           | non_empty_qitems qitems                              Grammar.seq
    qitem ::= item | (lid :: item)          give a name if used in the action
    item ::= '...'                                  Grammar.term(Lex.char ())
           | "..."                                Grammar.term(Lex.string ())
           | INT                                     Grammar.term(Lex.int ())
           | FLOAT                                 Grammar.term(Lex.float ())
           | RE(exp)        Grammar.term(Lex.regexp (Regexp.from_string exp))
           | exp                                                       itself
```

- non recursive let bindings correspond to just a name for the grammar.
- recursive let bindings correspond either to
  - [Grammar.declare_grammar + Grammar.set_grammar] (if no paramater)
  - [Grammar.grammar_familly + setting the grammar] is a parameter is given.

Anything which does not coresponds to this grammar will we keeped unchanged
in the structure as ocaml code (like the type definition in the example
above.  A mutually recursive definition can also mix te definition of
grammars (parametric of not) with the definition of normal ocaml values.

## Limitations

Pacomb must eliminate left recursion in grammars in order to use combinators
that would loop otherwise. However, left recursion is not support it it traverses:

- A [Grammar.layout] contructor to change blanks (probably possible to solve this,
  but probably not woth it.

- A [Grammar.desq] constructor that provides dependent sequence. Solving this
  is an open problem.

- Grammars are not left factorised automatically: (A B) | (A C) may parse A twice.
  two solutions
  - left factorise your grammar yourself,
  - Use Grammar.cache for A trading memory for speed.

- The ppx extension is not not too bad but still suffer from the fact that is uses
  a sublanguage of OCaml to describe grammar. For instance [%grammar (_::INT) => 0]
  is not legal because "_" can not be used in an Ocaml expression.
