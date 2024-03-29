PaComb: an efficient parsing library for OCaml
==============================================

PaComb implements a representation of grammars with semantic actions (values
returned as a result of parsing). Parsing is performed by compiling grammars
defined with the `Grammar` module (or indirectly though a PPX extension) to
the combinators of the `Combinator` module. The library offers _scanner less_
parsing, but the `Lex` module provide a notion of _terminals_ and _blanks_
that give a simple way to write grammars in two phases, as usual.

The main advantage of PaComb and similar solutions, contrary to ocamlyacc, is
that grammars (compiled or not) are __first class values__. This allows using
the full power of OCaml for manipulating grammars. For example, this is very
useful when working with syntax extension mechanisms.

Importantly, the __performances__ of PaComb are very good: it is only two to
five times slower than grammars generated by ocamlyacc, which is a compiler.

Defining languages using the `Grammar` module directly is cumbersome. For that
reason, PaComb provides a BNF-like PPX syntax extension (enabled using the
`-ppx pacomb.ppx` compilation flag).

A complete documentation is available via ocamldoc (make doc)

Pacomb also support: self extensible grammars, ambiguous grammars (with merge),
late rejection of rule via raising exception from action code, priority and others.

A complete [documentation is available](https://raffalli.eu/pacomb/pacomb)

As teaser, the usual calculator example:

```
(* The three levels of priorities *)
type p = Atom | Prod | Sum

let%parser rec
     (* This includes each priority level in the next one *)
     expr p = Atom < Prod < Sum
            (* all other rule are selected by their priority level *)
            ; (p=Atom) (x::FLOAT)                        => x
            ; (p=Atom) '(' (e::expr Sum) ')'             => e
            ; (p=Prod) (x::expr Prod) '*' (y::expr Atom) => x*.y
            ; (p=Prod) (x::expr Prod) '/' (y::expr Atom) => x/.y
            ; (p=Sum ) (x::expr Sum ) '+' (y::expr Prod) => x+.y
            ; (p=Sum ) (x::expr Sum ) '-' (y::expr Prod) => x-.y
̀̀̀
