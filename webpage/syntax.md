---
layout: default
---

## The Datalog core Syntax
In the Datalog program supported by BIRDS, a predicate `R` preceded by a symbol `+`/`-` (called a delta predicate) corresponds to the set of tuples that need to be inserted/deleted into/from table `R`.
The core syntax for Datalog supported by BIRDS is the following:
```text
<program> ::= {<statement>} <statement>
<statement> ::= <schema> | <constraint> | <rule>
<schema> ::= "source:" <relname> "(" <attribute>:<type> {"," <attribute>:<type>} ")" "." 
 | "view:" <relname> "(" <attribute>:<type> {"," <attribute>:<type>} ")" "."
<rule> ::= <predicate> ":-" <literal> { ("and"| ",") <literal> } "."
<constraint>::= ("⊥" | "_|_") ":-" <literal> { ("and"| ",") <literal> } "."
<literal> ::= <predicate> | ("not" | "¬") <predicate> | <builtin> | "not" <builtin>
<predicate> ::= [ ("+" | "-") ] <relname> "(" <term> {"," <term>} ")"
<builtin> ::= <varname> ("=" | "<>" | "<" | ">" | "<=" | ">=") <const>
<term> ::= <varname> | <anonvar> | <const>
<varname> ::= 'A'|..|'Z' { ('A'|..|'Z'| '0'|..|'9'|'_') }
<relname> ::= 'a'|..|'z'|'_' { ('a'|..|'z'| '0'|..|'9'|'_') }
<attribute> ::= 'a'|..|'z'|'_' { ('a'|..|'z'| '0'|..|'9'|'_') }
<type> ::= int | float | string
<anonvar> ::= '_'
<const> ::= <integer> | <float> | <string>
```

In general, the verification in BIRDS is sound. BIRDS guarantees the completeness of the verification if all Datalog rules are negation guarded. 

## Abbreviated syntax

### Primary key

BIRDS provides a shorthand syntax to declare primary key on relations.
Given a relation t(**A**, B, C), we can declare **A** as a primary key of t by:

```prolog
PK(t,[A])
```
That is an abbreviation for the following rules:

```prolog
% for the functional dependency A -> B:
_|_ :- t(A,B1,_), t(A,B2,_), not B1 = B2.
% for the functional dependency A -> C:
_|_ :- t(A,_,C1), t(A,_,C2), not C1 = C2.
``` 

Consider a relation s(**A**, **B**, C, D), where two columns **A** and **B** form the primary key of s(**A**, **B**, C, D).
We declare this constraint by:

```prolog
PK(t,[A,B])
```
That is an abbreviation for the following rules:

```prolog
% for the functional dependency A,B -> C:
_|_ :- s(A,B,C1,_), s(A,B,C2,_), not C1 = C2.
% for the functional dependency A,B -> D:
_|_ :- s(A,B,_,D1), s(A,B,_,D2), not D1 = D2.
``` 