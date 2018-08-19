---
layout: default
---

## View Update Datalog Syntax

```text
<automaton>  ::= <trans> <inits> <acceptings>
<program> ::= {<statement>}
<statement> ::= <rule> | <fact> | <query>
<rule> ::= <predicate> ":-" <literal> { ("and", ",") <literal> }
<literal> ::= | <predicate> | "not" <predicate> | <builtin> | "not" <builtin>
<predicate> ::= [ ("+" | "-") ] <relname> "(" <variable> {"," <variable>} ")"
<builtin> ::= <varname> ("=" | "<>" | "<" | ">" | "<=" | ">=") <const>
<variable> ::= <varname> | <anonvar> | <const>
<varname> ::= 'A'|..|'Z' { ('A'|..|'Z'| '0'|..|'9'|'_') }
<relname> ::= 'a'|..|'z' { ('a'|..|'z'| '0'|..|'9'|'_') }
<anonvar> ::= '_'
<const> ::= <integer> | <float> | <string>
```