---
layout: default
---

# BIRDS

## Overview

BIRDS (Putback-based **BI**directional transformation for **R**elational view update **D**atalog-based **S**trategies) is a bidirectional transformation (BX) framework for Relational Database Management Systems (RDBMS). The objective of this framework is to allow programmers to define a relational view in RDBMS by writing an update strategy for it.

## View Update Language: Datalog

BIRDS employs and extends the power of Datalog, which is a well known query language in RDBMS, in writing arbitrary update strategies for any relational views to source relations. Based on putback-based BX foundations, BIRDS automatically verifies the validity of a Datalog-written update strategy for a view (Putback direction) and then derive the definition of that view (Get direction) as a query over source relations. Finally, both Datalog-written update strategy and derived view definition are translated to a set of SQL statements with PL/pgSQL procedures, which can run directly in a PostgreSQL ORDBMS to create a new view along with its trigger.

<!-- The syntax for Datalog in writing relational view update strategies is discribed in [Update Datalog Syntax](syntax.html) -->

<!-- A view should be defined through a view update strategy to the base relations rather than a query over them. -->

<!-- {% include_relative syntax.md %} -->

## Datalog Syntax for Relational View Update

```text
<program> ::= {<statement>}
<statement> ::= <rule> | <query> | <base_relation>
<rule> ::= <predicate> ":-" <literal> { ("and"| ",") <literal> } "."
<base_relation> ::= "%s:" <predicate> "."
<query> ::= ("%v:" | "?-") <predicate> "."
<literal> ::= | <predicate> | "not" <predicate> | <builtin> | "not" <builtin>
<predicate> ::= [ ("+" | "-") ] <relname> "(" <variable> {"," <variable>} ")"
<builtin> ::= <varname> ("=" | "<>" | "<" | ">" | "<=" | ">=") <const>
<variable> ::= <varname> | <anonvar> | <const>
<varname> ::= 'A'|..|'Z' { ('A'|..|'Z'| '0'|..|'9'|'_') }
<relname> ::= 'a'|..|'z' { ('a'|..|'z'| '0'|..|'9'|'_') }
<anonvar> ::= '_'
<const> ::= <integer> | <float> | <string>
```

## Installation and Usage

### Installation

* Download executable files:
  * [For Macos]({{ site.github.mac_exe }})
  * [For Ubuntu]({{ site.github.ubuntu_exe }})
  <!-- * [Older versions](https://github.com/dangtv/BIRDS/releases) -->
* Build an executable file from the source code available at [https://github.com/dangtv/BIRDS](https://github.com/dangtv/BIRDS) (along with instructions)

### Usage

* The executable file is `birds`:

    ```bash
    birds [OPTIONS]
    -db         : print debugging information
    -f file     : read program from file, if not chosen, read from stdin
    -o file     : write program out file, if not chosen, print to stdout
    -s schema_name     : database schema name to connect to (default: public)
    -help  Display this list of options
    --help  Display this list of options
    ```

* For example, this command will transform the update strategy discribed in `test1.dl` to sql statements in `test1.sql` for creating a view with its triggers
    ```bash
    birds -s public -f test1.dl -o test1.sql
    ```

## Tutorials: defining updatable views with Datalog in BIRDS

* [The basics](basic-tutorial.html)
* [Examples on a music schema](music-tutorial.html)
* [Examples on a ride-sharing schema](ridesharing-tutorial.html)