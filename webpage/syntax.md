---
layout: default
---

## The Datalog core Syntax
In Datalog program supported by BIRDS, a predicate `R` preceded by a symbol `+`/`-` (called a delta predicate) corresponds to the set of tuples need to be inserted/deleted into/from table `R`.
The core syntax for Datalog supported by BIRDS is the following:
```text
<program>   ::= {<statement>} <statement>
<statement> ::= <schema> | <constraint> | <rule>
<schema>    ::= "source:" <relname> "(" <attr>:<type> {"," <attr>:<type>} ")" "." 
                | "view:" <relname> "(" <attr>:<type> {"," <attr>:<type>} ")" "."
<rule>      ::= <predicate> ":-" <literal> { ("and"| ",") <literal> } "."
<constraint>::= ("⊥" | "_|_") ":-" <literal> { ("and"| ",") <literal> } "."
<literal>   ::= <predicate> | ("not" | "¬") <predicate> | <builtin> | "not" <builtin>
<predicate> ::= [ ("+" | "-") ] <relname> "(" <term> {"," <term>} ")"
<builtin>   ::= <varname> ("=" | "<>" | "<" | ">" | "<=" | ">=") <const>
<term>      ::= <varname> | <anonvar> | <const>
<varname>   ::= 'A'|..|'Z' { ('A'|..|'Z'| '0'|..|'9'|'_') }
<relname>   ::= 'a'|..|'z'|'_' { ('a'|..|'z'| '0'|..|'9'|'_') }
<attr>      ::= 'a'|..|'z'|'_' { ('a'|..|'z'| '0'|..|'9'|'_') }
<type>      ::= int | float | string
<anonvar>   ::= '_'
<const>     ::= <integer> | <float> | <string>
```

In general, the verification in BIRDS is sound. BIRDS guarantees the completeness of the verification if all Datalog rules are negation guarded. 